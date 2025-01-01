#![allow(unused)]
use core::fmt;
use core::fmt::Debug;
use core::fmt::Display;
use core::str;

use anyhow::Context;

/// High level beatmap parser struct
///
/// Configure with [`BeatmapParser::new`] and then parse with [`BeatmapParser::parse`]
/// or [`BeatmapParser::parse_debug`].
pub struct BeatmapParser<'source> {
    /// beatmap difficulty .osu file. char slice instead of string for Unicode support.
    pub(crate) source: &'source [u8],
    /// parsed items. Currently stored as a debug item for checking where the item came from
    pub(crate) items: Vec<DebugItem<'source>>,
    /// parser mode, verbose or silent
    pub(crate) mode: ParserMode,
    /// start of the current item to be parsed
    pub(crate) start: usize,
    /// current index in the source
    pub(crate) index: usize,
    /// current line in the source
    pub(crate) line: usize,
    /// current column in the source
    pub(crate) column: usize,
    /// current section
    pub(crate) section: Section,
}

/// # Safety
///
/// The byte slice must be valid utf-8.
unsafe trait ConvertByteSliceToStrSlice<'a> {
    /// # Safety
    ///
    /// The byte slice must be valid utf-8.
    unsafe fn convert_byte_slice_to_str_slice(self) -> &'a str;

    /// # Safety
    ///
    /// The byte slice must be valid utf-8.
    unsafe fn convert_byte_slice_to_string(self) -> String;
}

unsafe impl<'a> ConvertByteSliceToStrSlice<'a> for &'a [u8] {
    #[inline]
    unsafe fn convert_byte_slice_to_str_slice(self) -> &'a str {
        // str::from_utf8(self).context("converting u8 slice to string").unwrap()
        unsafe { str::from_utf8_unchecked(self) }
    }

    #[inline]
    unsafe fn convert_byte_slice_to_string(self) -> String {
        let v = self.to_vec();
        unsafe { String::from_utf8_unchecked(v) }
        // String::from_utf8(v).context("converting u8 slice to string").unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Section {
    Beginning,
    General,
    // Editor,
    // Metadata,
    // Difficulty,
    // Events,
    TimingPoints,
    HitObjects,
    /// This mode only parses section headers and tries to set the parser
    /// section to the correct section. This is required for supporting unknown
    /// sections.
    OnlySectionHeaders,
}

impl Section {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Beginning => "Beginning",
            Self::General => "General",
            // Self::Editor => "Editor",
            // Self::Metadata => "Metadata",
            // Self::Difficulty => "Difficulty",
            // Self::Events => "Events",
            Self::TimingPoints => "TimingPoints",
            Self::HitObjects => "HitObjects",
            Self::OnlySectionHeaders => "OnlySectionHeaders",
        }
    }

    #[inline]
    pub fn from_header(header: &str) -> Option<Self> {
        match header {
            "[General]" => Some(Self::General),
            // "[Editor]" => Some(Self::Editor),
            // "[Metadata]" => Some(Self::Metadata),
            // "[Difficulty]" => Some(Self::Difficulty),
            // "[Events]" => Some(Self::Events),
            "[TimingPoints]" => Some(Self::TimingPoints),
            "[HitObjects]" => Some(Self::HitObjects),
            _ => None,
        }
    }
}

impl Display for Section {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Verbose mode will print out errors and warnings
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParserMode {
    Verbose,
    Silent,
}

impl Debug for BeatmapParser<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("source", &"...")
            .field("items", &format!("... (length: {})", self.items.len()))
            .field("mode", &self.mode)
            .field("start", &self.start)
            .field("index", &self.index)
            .field("line", &self.line)
            .field("column", &self.column)
            .finish()
    }
}

macro_rules! write_loop_impl {
    ($fn_name:ident, $parse_fn_name:ident) => {
        #[inline]
        fn $fn_name(&mut self) -> ParserControlFlow {
            while !self.is_at_end() {
                self.start = self.index;

                // self.print_info(&format!("SECTION {} in {}", self.section, stringify!($fn_name)));
                // Safety: the while loop condition guarantees that we are not at the end
                let ret = unsafe { self.$parse_fn_name() };

                match ret {
                    ParserControlFlow::ChangeParseLoop => {
                        return ParserControlFlow::ChangeParseLoop;
                    }

                    ParserControlFlow::ContinueParseLoop => {
                        continue;
                    }

                    ParserControlFlow::ParserFinished => {
                        return ParserControlFlow::ParserFinished;
                    }
                };
            }

            ParserControlFlow::ParserFinished
        }
    };
}

impl<'source> BeatmapParser<'source> {
    #[inline]
    pub(crate) const fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }

    /// Safety: [`Self::is_at_end`] must be false. Consumes the current character.
    #[inline]
    #[track_caller]
    pub(crate) unsafe fn advance(&mut self) -> u8 {
        debug_assert!(self.index < self.source.len(), "len is {} but index is {}", self.source.len(), self.index);
        let current = unsafe { *self.source.get_unchecked(self.index) };
        // if current == b'\n' {
        //     self.line += 1;
        //     self.column = 0;
        // } else {
        //     self.column += 1;
        // }
        self.index += 1;
        current
    }

    /// Consumes the current character if it matches the expected character.
    #[inline]
    pub(crate) fn r#match(&mut self, expected: u8) -> bool {
        if self.is_at_end()
            // Safety: control flow of the `||` operator guarantees that we are not at the end
            || unsafe { *self.source.get_unchecked(self.index) } != expected
        {
            false
        } else {
            self.index += 1;
            true
        }
    }

    /// 1 character lookahead. Does not consume the current character.
    ///
    /// # Safety
    ///
    /// [`Self::is_at_end`] must be false
    #[inline]
    pub(crate) unsafe fn peek(&self) -> u8 {
        unsafe { *self.source.get_unchecked(self.index) }
    }

    /// 2 character lookahead. Does not consume the current character.
    #[inline]
    pub(crate) fn peek_next(&self) -> Option<u8> {
        if self.index.checked_add(1)? >= self.source.len() {
            None
        } else {
            // Safety: we have just checked that self.index + 1 is not out of bounds
            Some(unsafe { *self.source.get_unchecked(self.index + 1) })
        }
    }

    #[inline]
    #[cold]
    #[track_caller]
    pub(crate) fn print_error_with_context(&self, message: &str) {
        if self.mode == ParserMode::Verbose {
            eprintln!(
                "unexpected error at {},{} with message: {}\n\tsource: {}",
                self.line,
                self.column,
                message,
                String::from_utf8(self.source[self.start..self.index].to_vec())
                    .context("converting u8 slice to string")
                    .unwrap()
            );
        }
    }

    /// Disabled for performance.
    #[inline]
    pub(crate) const fn print_info(&self, message: &str) {
        // if self.mode == ParserMode::Verbose {
        //     eprintln!("INFO {},{}: {}", self.line, self.column, message);
        // }
    }

    /// Pushes the given item into the items vector. The source is automatically
    /// sliced using the start and index fields.
    #[inline]
    pub(crate) fn push_item(&mut self, item: Item<'source>) {
        let source_slice = &self.source[self.start..self.index];
        let source = unsafe { source_slice.convert_byte_slice_to_str_slice() };
        let debug_item = DebugItem { source, item };
        self.items.push(debug_item);
    }

    /// Parses a single integer value.
    #[inline]
    pub(crate) fn parse_integer(&mut self) {
        while !self.is_at_end() {
            // Safety: we have just checked that we are not at the end
            let c = unsafe { self.peek() };

            if self.is_at_end() {
                break;
            }

            if c != b'-' && !c.is_ascii_digit() {
                break;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        let integer_str = &self.source[self.start..self.index];
        let Ok(integer) = unsafe { integer_str.convert_byte_slice_to_str_slice() }.parse::<isize>() else {
            self.print_error_with_context("parsing integer");
            return;
        };
        self.push_item(Item::Integer(integer));
    }

    /// Parses a single float, delimited by a period.
    #[inline]
    pub(crate) fn parse_float(&mut self) {
        while !self.is_at_end() {
            // Safety: we have just checked that we are not at the end
            let c = unsafe { self.peek() };

            if c != b'-' && !c.is_ascii_digit() && c != b'.' {
                break;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        let float_str = &self.source[self.start..self.index];
        let Ok(float) = unsafe { float_str.convert_byte_slice_to_str_slice() }.parse::<f64>() else {
            self.print_error_with_context("parsing float");
            return;
        };
        self.push_item(Item::Float(float));
    }

    /// Parses any literal value. A literal is any value that is not a number or a quoted string.
    #[inline]
    pub(crate) fn parse_literal(&mut self) {
        while !self.is_at_end() {
            // Safety: we have just checked that we are not at the end
            let c = unsafe { self.peek() };

            if matches!(c, b'\r' | b'\n' | b'[' | b'"' | b',' | b':') {
                break;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        // do not include the newline character if it is the last character
        // todo: check if this works with `:`, `[`, `,`, and `"` too
        let literal_slice = if !self.is_at_end() && {
            // Safety: control flow of the `&&` operator guarantees that we are not at the end
            unsafe { self.peek() }
        } == b'\n'
        {
            &self.source[self.start..self.index - 1]
        } else {
            &self.source[self.start..self.index]
        };

        let literal = unsafe { literal_slice.convert_byte_slice_to_str_slice() };
        self.push_item(Item::Literal(literal));
    }

    /// Parses a section header. A section header is any string enclosed in square brackets.
    #[inline]
    pub(crate) fn parse_section_header(&mut self) -> Option<Section> {
        while !self.is_at_end() {
            // Safety: we have just checked that we are not at the end
            let c = unsafe { self.peek() };

            if c == b']' {
                break;
            }

            if !c.is_ascii_alphanumeric() {
                self.print_error_with_context("invalid character in section header");
                return None;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        if self.is_at_end() {
            self.print_error_with_context("end of file while parsing section header");
            return None;
        };

        // Consume the ending bracket
        // Safety: we early returned if we were at the end
        unsafe { self.advance() };

        let section_header_slice = &self.source[self.start..self.index];
        let section_header_string = section_header_slice.to_vec();
        let header_string = unsafe { section_header_string.convert_byte_slice_to_string() };
        let section = Section::from_header(&header_string);
        if let Some(sect) = section {
            self.push_item(Item::SectionHeader(sect));
        } else {
            let unkstr = unsafe { section_header_slice.convert_byte_slice_to_str_slice() };
            self.push_item(Item::UnknownSectionHeader(unkstr));
        }
        section
    }

    /// Parses a quoted string. A quoted string is any string enclosed in double quotes.
    #[inline]
    pub(crate) fn parse_quoted_string(&mut self) {
        while !self.is_at_end() {
            // Safety: we have just checked that we are not at the end
            let c = unsafe { self.peek() };

            if c == b'"' {
                break;
            }

            if c == b'\n' {
                self.line += 1;
                self.column = 0;
                break;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        if self.is_at_end() {
            self.print_error_with_context("unterminated quoted string");
            return;
        };

        let quoted_string_slice = &self.source[self.start..self.index];

        // Consume the ending quote
        // Safety: we early returned if we were at the end
        unsafe { self.advance() };

        let quoted_string_str = unsafe { quoted_string_slice.convert_byte_slice_to_str_slice() };

        self.push_item(Item::QuotedString(quoted_string_str));
    }

    /// Parses a value after a colon until the next newline character.
    #[inline]
    pub(crate) fn parse_until_newline(&mut self) {
        while !self.is_at_end() {
            // Safety: we have just checked that we are not at the end
            let c = unsafe { self.peek() };

            if matches!(c, b'\r' | b'\n') {
                break;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        let value_slice = if self.start == self.index {
            &self.source[self.start..self.start]
        } else {
            &self.source[self.start..self.index]
        };

        // Consume the newline character(s)
        // Safety: we early returned if we were at the end
        if !self.is_at_end() && unsafe { self.peek() } == b'\r' {
            unsafe { self.advance() };
        }
        if !self.is_at_end() && unsafe { self.peek() } == b'\n' {
            unsafe { self.advance() };
        }

        let value_str = unsafe { value_slice.convert_byte_slice_to_str_slice() };
        self.push_item(Item::ValueUntilNewline(value_str));
    }

    /// Parse a single hit object.
    #[inline]
    pub(crate) fn parse_single_hit_object(&mut self) {
        /* Hit objects

        Hit object syntax: x,y,time,type,hitSound,objectParams,hitSample

            x (Integer) and y (Integer): Position in osu! pixels of the object.
            time (Integer): Time when the object is to be hit, in milliseconds from the beginning of the beatmap's audio.
            type (Integer): Bit flags indicating the type of the object. See the type section.
            hitSound (Integer): Bit flags indicating the hitsound applied to the object. See the hitsound section.
            objectParams (Comma-separated list): Extra parameters specific to the object's type.
            hitSample (Colon-separated list): Information about which samples are played when the object is hit. It is closely
            related to hitSound; see the hitsounds section. If it is not written, it defaults to 0:0:0:0:.
        */

        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;

        // type
        self.parse_integer();
        self.start = self.index;
        // we don't care about the rest of the hit object (for now)
        self.parse_until_newline();
    }

    /// Parse a single timing point.
    #[inline]
    pub(crate) fn parse_single_timing_point(&mut self) {
        /* Timing points

        Each timing point influences a specified portion of the map, commonly called a "timing section".
        The .osu file format requires these to be sorted in chronological order.

        Timing point syntax: time,beatLength,meter,sampleSet,sampleIndex,volume,uninherited,effects

            time (Integer): Start time of the timing section, in milliseconds from the beginning of the beatmap's
                audio. The end of the timing section is the next timing point's time (or never, if this is the last timing point).
            beatLength (Decimal): This property has two meanings:
                For uninherited timing points, the duration of a beat, in milliseconds.
                For inherited timing points, a negative inverse slider velocity multiplier, as a percentage. For example,
                    -50 would make all sliders in this timing section twice as fast as SliderMultiplier.
            meter (Integer): Amount of beats in a measure. Inherited timing points ignore this property.
            sampleSet (Integer): Default sample set for hit objects (0 = beatmap default, 1 = normal, 2 = soft, 3 = drum).
            sampleIndex (Integer): Custom sample index for hit objects. 0 indicates osu!'s default hitsounds.
            volume (Integer): Volume percentage for hit objects.
            uninherited (0 or 1): Whether or not the timing point is uninherited.
            effects (Integer): Bit flags that give the timing point extra effects. See the effects section.
         */

        // time
        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        // beatLength
        self.parse_float();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        // meter
        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        // sampleSet
        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        // sampleIndex
        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        // volume
        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        // uninherited
        self.parse_integer();
        if !self.r#match(b',') {
            self.print_info("missing comma");
            return;
        };
        self.start = self.index;
        // effects
        self.parse_integer();
        self.start = self.index;
        // we don't care about the rest of the timing point (for now)
        self.parse_until_newline();
    }

    /// Parsing logic for the beginning of the file without any sections.
    ///
    /// # Safety
    ///
    /// [`Self::is_at_end`] must be false
    #[inline]
    pub(crate) unsafe fn parse_item_beginning(&mut self) -> ParserControlFlow {
        // Safety: caller upholds the contract
        let c = unsafe { self.advance() };

        match c {
            b'[' => {
                self.print_info("found [");
                if let Some(section) = self.parse_section_header() {
                    self.print_info(&format!("found section header: {}", section));
                    self.section = section;
                } else {
                    self.print_error_with_context("unknown section header");
                    self.section = Section::OnlySectionHeaders;
                };

                return ParserControlFlow::ChangeParseLoop;
            }

            b'o' => {
                const MAGIC: &[u8] = b"su file format v14";
                self.print_info("found 'o'");
                if self.source[self.index..].starts_with(MAGIC) {
                    self.index += MAGIC.len();
                    self.push_item(Item::FileFormatMagic);
                    self.print_info("found file format magic");
                } else {
                    self.print_error_with_context("invalid file format magic");
                }
            }

            b'\r' => {
                // ignore the carriage return
                self.print_info("found carriage return");
            }

            b'\n' => {
                self.print_info("found newline");
                self.column = 0;
                self.line += 1;
                self.push_item(Item::Newline);
            }

            _ => {
                self.print_error_with_context("invalid character");
            }
        };

        ParserControlFlow::ContinueParseLoop
    }

    /// Parsing logic that ignores everything but section headers.
    ///
    /// # Safety
    ///
    /// [`Self::is_at_end`] must be false
    #[inline]
    pub(crate) unsafe fn parse_item_only_section_headers(&mut self) -> ParserControlFlow {
        // Safety: caller upholds the contract
        let c = unsafe { self.advance() };

        #[allow(clippy::single_match_else)]
        match c {
            b'[' => {
                if let Some(section) = self.parse_section_header() {
                    self.section = section;
                } else {
                    self.print_error_with_context("unknown section header");
                    self.section = Section::OnlySectionHeaders;
                };
                return ParserControlFlow::ChangeParseLoop;
            }

            _ => {
                // ignore everything else
                self.parse_until_newline();
                self.start = self.index;
            }
        }
        ParserControlFlow::ContinueParseLoop
    }

    /// Parsing logic that ignores everything but section headers.
    ///
    /// # Safety
    ///
    /// [`Self::is_at_end`] must be false
    #[inline]
    pub(crate) unsafe fn parse_item_general(&mut self) -> ParserControlFlow {
        // Safety: caller upholds the contract
        let c = unsafe { self.advance() };

        match c {
            b'[' => {
                if let Some(section) = self.parse_section_header() {
                    self.section = section;
                } else {
                    self.print_error_with_context("unknown section header");
                    self.section = Section::OnlySectionHeaders;
                };
                return ParserControlFlow::ChangeParseLoop;
            }

            b'\r' => {
                // ignore the carriage return
            }

            b'\n' => {
                self.column = 0;
                self.line += 1;
                self.push_item(Item::Newline);
            }

            // somehow we are never reaching this case
            c if c.is_ascii_alphabetic() => {
                self.parse_literal();
            }

            b':' => {
                self.push_item(Item::Colon);
                if self.is_at_end() {
                    self.print_error_with_context("end of file after colon");
                    return ParserControlFlow::ParserFinished;
                }

                // Safety: we have just checked that we are not at the end
                unsafe { self.advance() };
                self.start = self.index;
                self.parse_until_newline();
            }

            _ => {
                self.print_error_with_context("invalid character");
            }
        };

        ParserControlFlow::ContinueParseLoop
    }

    /// Parse the next item in the source.
    ///
    /// # Safety
    ///
    /// [`Self::is_at_end`] must be false
    #[inline]
    pub(crate) unsafe fn parse_item_hit_objects(&mut self) -> ParserControlFlow {
        // Safety: caller upholds the contract
        let c = unsafe { self.advance() };

        match c {
            b'\r' => {
                // ignore the carriage return
            }

            b'\n' => {
                self.column = 0;
                self.line += 1;
                self.push_item(Item::Newline);
            }

            b'[' => {
                if let Some(section) = self.parse_section_header() {
                    self.section = section;
                } else {
                    self.print_error_with_context("unknown section header");
                    self.section = Section::OnlySectionHeaders;
                };
                return ParserControlFlow::ChangeParseLoop;
            }

            _ => {
                self.parse_single_hit_object();
            }
        };

        ParserControlFlow::ContinueParseLoop
    }

    /// Parse the next item in the source.
    ///
    /// # Safety
    ///
    /// [`Self::is_at_end`] must be false
    #[inline]
    pub(crate) unsafe fn parse_item_timing_points(&mut self) -> ParserControlFlow {
        // Safety: caller upholds the contract
        let c = unsafe { self.advance() };

        match c {
            b'\r' => {
                // ignore the carriage return
            }

            b'\n' => {
                self.column = 0;
                self.line += 1;
                self.push_item(Item::Newline);
            }

            b'[' => {
                if let Some(section) = self.parse_section_header() {
                    self.section = section;
                } else {
                    self.print_error_with_context("unknown section header");
                    self.section = Section::OnlySectionHeaders;
                };
                return ParserControlFlow::ChangeParseLoop;
            }

            _ => {
                self.parse_single_timing_point();
            }
        };

        ParserControlFlow::ContinueParseLoop
    }

    // /// Parse the next item in the source.
    // ///
    // /// # Safety
    // ///
    // /// [`Self::is_at_end`] must be false
    // #[inline]
    // pub(crate) unsafe fn parse_item(&mut self) {
    //     // Safety: caller upholds the contract
    //     let c = unsafe { self.advance() };

    //     match c {
    //         b'\r' => {
    //             // ignore the carriage return
    //         }

    //         b'\n' => {
    //             self.column = 0;
    //             self.line += 1;
    //             self.push_item(Item::Newline);
    //         }

    //         b'[' => {
    //             self.parse_section_header();
    //         }

    //         b'"' => {
    //             self.parse_quoted_string();
    //         }

    //         b'/' => {
    //             if self.r#match(b'/') {
    //                 while !self.is_at_end() {
    //                     // Safety: we have just checked that we are not at the end
    //                     let c = unsafe { self.peek() };

    //                     if c == b'\n' {
    //                         break;
    //                     }

    //                     // Safety: we have just checked that we are not at the end
    //                     unsafe { self.advance() };
    //                 }
    //             }
    //         }

    //         b':' => {
    //             self.push_item(Item::Colon);
    //             if self.is_at_end() {
    //                 self.print_error_with_context("end of file after colon");
    //                 return;
    //             }

    //             // Safety: we have just checked that we are not at the end
    //             unsafe { self.advance() };
    //             self.start = self.index;
    //             self.parse_until_newline();
    //         }

    //         c if !c.is_ascii_digit() => {
    //             self.parse_literal();
    //         }

    //         _ => {
    //             self.print_error_with_context("invalid character");
    //         }
    //     };
    // }

    write_loop_impl! {loop_beginning, parse_item_beginning}
    write_loop_impl! {loop_only_section_headers, parse_item_only_section_headers}
    write_loop_impl! {loop_general, parse_item_general}
    write_loop_impl! {loop_hit_objects, parse_item_hit_objects}
    write_loop_impl! {loop_timing_points, parse_item_timing_points}

    #[inline]
    pub(crate) fn parse_generic(&mut self) {
        loop {
            let res = match self.section {
                // meta sections
                Section::Beginning => self.loop_beginning(),
                Section::OnlySectionHeaders => self.loop_only_section_headers(),

                // real sections
                Section::General => self.loop_general(),
                Section::HitObjects => self.loop_hit_objects(),
                Section::TimingPoints => self.loop_timing_points(),
                // TODO: support more sections
                // _ => break;,
            };

            match res {
                ParserControlFlow::ContinueParseLoop | ParserControlFlow::ChangeParseLoop => {
                    continue;
                }

                ParserControlFlow::ParserFinished => {
                    break;
                }
            }
        }
    }

    /// Construct a new parser with the given source and mode.
    #[inline]
    #[must_use = "the parser is lazy and does not parse until you call [`BeatmapParser::parse`]."]
    pub fn new(source: &'source str, mode: ParserMode) -> Self {
        Self {
            source: source.as_bytes(),
            items: Vec::with_capacity(/* hint */ source.len() / 6),
            mode,
            start: 0,
            index: 0,
            line: 1,
            column: 0,
            section: Section::Beginning,
        }
    }

    /// Parse the source into a vector of items. This function consumes the parser.
    #[inline]
    #[must_use]
    #[no_mangle]
    pub fn parse(mut self) -> Vec<Item<'source>> {
        self.parse_generic();
        self.items.into_iter().map(|item| item.item).collect()
    }

    /// Parse the source into a vector of debug items. This function consumes the parser.
    #[inline]
    #[must_use]
    pub fn parse_debug(mut self) -> Vec<DebugItem<'source>> {
        self.parse_generic();
        self.items
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParserControlFlow {
    ParserFinished,
    ChangeParseLoop,
    ContinueParseLoop,
}

/// An item with the source annotated.
#[derive(Debug)]
pub struct DebugItem<'source> {
    pub source: &'source str,
    pub item: Item<'source>,
}

#[derive(PartialEq)]
pub enum Item<'source> {
    Comma,
    Colon,
    Newline,
    FileFormatMagic,
    Literal(&'source str),
    Integer(isize),
    Float(f64),
    QuotedString(&'source str),
    ValueUntilNewline(&'source str),
    SectionHeader(Section),
    UnknownSectionHeader(&'source str),
}

impl Debug for Item<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Comma => write!(f, "Comma"),
            Self::Colon => write!(f, "Colon"),
            Self::Newline => write!(f, "Newline"),
            Self::FileFormatMagic => write!(f, "FileFormatMagic"),
            Self::Literal(lit) => write!(f, "Literal({:?})", lit),
            Self::Integer(int) => write!(f, "Integer({:?})", int),
            Self::Float(float) => write!(f, "Float({:?})", float),
            Self::QuotedString(quoted) => write!(f, "QuotedString({:?})", quoted),
            Self::ValueUntilNewline(value) => write!(f, "ValueUntilNewline({:?})", value),
            Self::SectionHeader(section) => write!(f, "SectionHeader({:?})", section),
            Self::UnknownSectionHeader(unk) => write!(f, "UnknownSectionHeader({:?})", unk),
        }
    }
}
