#![feature(try_blocks)]
extern crate anyhow;

use core::fmt;
use core::fmt::Debug;

/*
 * Each timing point influences a specified portion of the map, commonly called a "timing section".
 * The .osu file format requires these to be sorted in chronological order.
 *
 * Timing point syntax: time,beatLength,meter,sampleSet,sampleIndex,volume,uninherited,effects
 *
 * time (Integer): Start time of the timing section, in milliseconds from the beginning of the beatmap's audio.
 *     The end of the timing section is the next timing point's time (or never, if this is the last timing point).
 * beatLength (f64): This property has two meanings:
 *     For uninherited timing points, the duration of a beat, in milliseconds.
 *     For inherited timing points, a negative inverse slider velocity multiplier, as a percentage. For example,
 *         -50 would make all sliders in this timing section twice as fast as SliderMultiplier.
 * meter (Integer): Amount of beats in a measure. Inherited timing points ignore this property.
 * sampleSet (Integer): Default sample set for hit objects (0 = beatmap default, 1 = normal, 2 = soft, 3 = drum).
 * sampleIndex (Integer): Custom sample index for hit objects. 0 indicates osu!'s default hitsounds.
 * volume (Integer): Volume percentage for hit objects.
 * uninherited (0 or 1): Whether or not the timing point is uninherited.
 * effects (Integer): Bit flags that give the timing point extra effects. See the effects section.
 */
pub struct TimingPoint {
    time: isize,
    beat_length: f64,
    inherited: TimingPointType,
}

pub enum TimingPointType {
    Uninherited,
    Inherited,
}

pub struct BeatmapParser {
    source: Vec<char>,
    items: Vec<DebugItem>,
    mode: ParserMode,
    start: usize,
    index: usize,
    line: usize,
    column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParserMode {
    Verbose,
    Silent,
}

impl Debug for BeatmapParser {
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

impl BeatmapParser {
    const GENERAL_SECTION_HEADER: &'static str = "[General]";

    // // TODO: parse editor section
    // const EDITOR_SECTION_HEADER: &'static str = "[Editor]";

    // // TODO parse metadata section
    // const METADATA_SECTION_HEADER: &'static str = "[Metadata]";

    // // TODO parse difficulty section
    // const DIFFICULTY_SECTION_HEADER: &'static str = "[Difficulty]";

    // // TODO parse events section
    // const COLORS_SECTION_HEADER: &'static str = "[Colours]";

    const TIMING_POINT_SECTION_HEADER: &'static str = "[TimingPoints]";
    const HIT_OBJECT_SECTION_HEADER: &'static str = "[HitObjects]";

    const VALID_HEADERS: [&'static str; 3] = [
        Self::GENERAL_SECTION_HEADER,
        Self::TIMING_POINT_SECTION_HEADER,
        Self::HIT_OBJECT_SECTION_HEADER,
    ];

    #[inline]
    fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }

    /// Safety: [`Self::is_at_end`] must be false
    #[inline]
    unsafe fn advance(&mut self) -> char {
        debug_assert!(self.index < self.source.len(), "len is {} but index is {}", self.source.len(), self.index);
        let current = unsafe { self.source.get_unchecked(self.index) };
        self.index += 1;
        self.column += 1;
        *current
    }

    #[inline]
    fn push_item(&mut self, item: Item) {
        let source = self.source[self.start..self.index].iter().collect::<String>();
        let debug_item = DebugItem { source, item };
        self.items.push(debug_item);
    }

    #[inline]
    fn r#match(&mut self, expected: char) -> bool {
        if self.is_at_end() || unsafe { *self.source.get_unchecked(self.index) } != expected {
            false
        } else {
            self.index += 1;
            true
        }
    }

    #[inline]
    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            Some(unsafe { *self.source.get_unchecked(self.index) })
        }
    }

    #[inline]
    fn peek_next(&self) -> Option<char> {
        if self.index + 1 >= self.source.len() {
            None
        } else {
            Some(unsafe { *self.source.get_unchecked(self.index + 1) })
        }
    }

    #[inline]
    fn parse_literal(&mut self) {
        while let Some(c) = self.peek() {
            if self.is_at_end() {
                break;
            }

            if matches!(c, '\r' | '\n' | '[' | '"' | ',' | ':') {
                break;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        if self.is_at_end() {
            if self.mode == ParserMode::Verbose {
                eprint!(
                    "unexpected end of file at {},{}\n\tliteral: {}\n",
                    self.line,
                    self.column,
                    self.source[self.start..self.index].iter().collect::<String>()
                );
            }
            return;
        };

        // do not include the newline character if it is the last character
        let literal = if Some('\n') == self.peek() {
            self.source[self.start..self.index - 1].iter().collect::<String>()
        } else {
            self.source[self.start..self.index].iter().collect::<String>()
        };

        self.push_item(Item::Literal(literal));
    }

    #[inline]
    fn parse_section_header(&mut self) {
        // section header syntax is:
        // [sectionname]

        while let Some(c) = self.peek() {
            if self.is_at_end() {
                break;
            }

            if c == ']' {
                break;
            }

            if !c.is_alphanumeric() {
                if self.mode == ParserMode::Verbose {
                    eprint!(
                        "unexpected character while parsing section header at {},{}\n\tsection header: {}\n",
                        self.line,
                        self.column,
                        self.source[self.start..self.index].iter().collect::<String>()
                    );
                }
                return;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        if self.is_at_end() {
            if self.mode == ParserMode::Verbose {
                eprint!(
                    "unexpected end of file at {},{}\n\tsection header: {}\n",
                    self.line,
                    self.column,
                    self.source[self.start..self.index].iter().collect::<String>()
                );
            }
            return;
        };

        // Consume the ending bracket
        // Safety: we early returned if we were at the end
        unsafe { self.advance() };

        let section_header = self.source[self.start..self.index].iter().collect::<String>();
        self.push_item(Item::SectionHeader(section_header));
    }

    #[inline]
    fn parse_quoted_string(&mut self) {
        while let Some(c) = self.peek() {
            if self.is_at_end() {
                break;
            }

            if c == '"' {
                break;
            }

            if c == '\n' {
                self.line += 1;
                self.column = 0;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        if self.is_at_end() {
            if self.mode == ParserMode::Verbose {
                eprint!(
                    "unexpected end of file at {},{}\n\tquoted string: {}\n",
                    self.line,
                    self.column,
                    self.source[self.start..self.index].iter().collect::<String>()
                );
            }
            return;
        };

        // Consume the ending quote
        // Safety: we early returned if we were at the end
        unsafe { self.advance() };

        let quoted_string = self.source[self.start..self.index].iter().collect::<String>();
        self.push_item(Item::QuotedString(quoted_string));
    }

    /// parse the value after the colon, syntax:
    /// <previous things>: value
    #[inline]
    fn parse_value_after_colon(&mut self) {
        while let Some(c) = self.peek() {
            if self.is_at_end() {
                break;
            }

            if matches!(c, '\r' | '\n') {
                break;
            }

            // Safety: we have just checked that we are not at the end
            unsafe { self.advance() };
        }

        if self.is_at_end() {
            if self.mode == ParserMode::Verbose {
                eprint!(
                    "unexpected end of file at {},{}\n\tvalue after colon: {}\n",
                    self.line,
                    self.column,
                    self.source[self.start..self.index].iter().collect::<String>()
                );
            }
            return;
        };

        let value = self.source[self.start..self.index].iter().collect::<String>();
        self.push_item(Item::ValueAfterColon(value));
    }

    /// Safety: [`Self::is_at_end`] must be false
    #[inline]
    unsafe fn parse_item(&mut self) {
        // Safety: caller upholds the contract
        let c = unsafe { self.advance() };

        match c {
            '\r' => {
                // ignore the carriage return
            }

            '\n' => {
                self.column = 0;
                self.line += 1;
                self.push_item(Item::Newline);
            }

            '[' => {
                self.parse_section_header();
            }

            '"' => {
                self.parse_quoted_string();
            }

            ':' => {
                self.push_item(Item::Colon);
                if self.is_at_end() {
                    if self.mode == ParserMode::Verbose {
                        eprint!(
                            "unexpected end of file at {},{}\n\tcolon: {}\n",
                            self.line,
                            self.column,
                            self.source[self.start..self.index].iter().collect::<String>()
                        );
                    }
                    return;
                }

                // Safety: we have just checked that we are not at the end
                unsafe { self.advance() };
                self.start = self.index;
                self.parse_value_after_colon();
            }

            c if !c.is_numeric() => {
                self.parse_literal();
            }

            _ => {
                if ParserMode::Verbose == self.mode {
                    eprint!(
                        "invalid item at {},{}\n\tliteral: {}\n",
                        self.line,
                        self.column,
                        self.source[self.start..self.index].iter().collect::<String>()
                    );
                }
            }
        };
    }

    #[inline]
    #[must_use = "the parser is lazy and does not parse until you call [`BeatmapParser::parse`]."]
    pub fn new(source: &str, mode: ParserMode) -> Self {
        Self {
            source: source.chars().collect::<Vec<_>>(),
            items: Vec::new(),
            mode,
            start: 0,
            index: 0,
            line: 1,
            column: 0,
        }
    }

    #[inline]
    #[must_use]
    pub fn parse(mut self) -> Vec<Item> {
        while !self.is_at_end() {
            self.start = self.index;
            // Safety: we have just checked that we are not at the end
            unsafe { self.parse_item() };
        }
        self.items.into_iter().map(|item| item.item).collect()
    }

    #[inline]
    #[must_use]
    pub fn parse_debug(mut self) -> Vec<DebugItem> {
        while !self.is_at_end() {
            self.start = self.index;
            // Safety: we have just checked that we are not at the end
            unsafe { self.parse_item() };
        }
        self.items
    }
}

#[derive(Debug)]
pub struct DebugItem {
    source: String,
    item: Item,
}

#[derive(Debug)]
pub enum Item {
    Comma,
    Colon,
    Newline,
    ValueAfterColon(String),
    Literal(String),
    Integer(String),
    Float(String),
    QuotedString(String),
    SectionHeader(String),
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    use super::*;
    use std::{
        fs::File,
        io::{BufWriter, Write},
    };

    use anyhow::{Context, Result};

    macro_rules! write_test_fn_for_filename {
        ($fn_name:ident, $in_path:literal, $out_path:literal) => {
            #[test]
            fn $fn_name() {
                const TEST_BEATMAP_1: &str = include_str!($in_path);
                let parser = BeatmapParser::new(TEST_BEATMAP_1, ParserMode::Silent);
                let items = parser.parse_debug();

                let len = items.len();
                let res: Result<()> = try {
                    let f = File::create($out_path).context("creating test output file")?;
                    let mut writer = BufWriter::new(f);
                    for item in items {
                        let DebugItem { source: src, item } = item;
                        match item {
                            Item::Literal(lit) => {
                                writer
                                    .write_all(format!("{src:?} => Literal({lit:?})\n").as_bytes())
                                    .context("writing to file")?;
                            }

                            Item::Integer(lit) => {
                                writer
                                    .write_all(format!("{src:?} => Integer({lit:?}\n").as_bytes())
                                    .context("writing to file")?;
                            }

                            Item::Float(lit) => {
                                writer
                                    .write_all(format!("{src:?} => Float({lit:?})\n").as_bytes())
                                    .context("writing to file")?;
                            }

                            Item::QuotedString(lit) => {
                                writer
                                    .write_all(format!("{src:?} => QuotedString({lit:?})\n").as_bytes())
                                    .context("writing to file")?;
                            }

                            Item::Newline => {
                                writer
                                    .write_all(format!("{src:?} => Newline\n").as_bytes())
                                    .context("writing to file")?;
                            }

                            _ => {
                                writer
                                    .write_all(format!("\"{src}\" => {:?}\n", item).as_bytes())
                                    .context("writing to file")?;
                            }
                        }
                    }
                };

                match res {
                    Ok(_) => {}
                    Err(e) => {
                        panic!("actual panic: {}", e);
                    }
                }

                panic!("test output panic: {:#?}", len);
            }
        };
    }

    write_test_fn_for_filename!(test_beatmap_1, r"..\tests\testbeatmap1.osu", r".\tests\testbeatmap1.osu.out");
    write_test_fn_for_filename!(test_beatmap_2, r"..\tests\testbeatmap2.osu", r".\tests\testbeatmap2.osu.out");
    write_test_fn_for_filename!(test_beatmap_3, r"..\tests\testbeatmap3.osu", r".\tests\testbeatmap3.osu.out");
}
