use super::parser::*;
use std::{
    fs::File,
    io::{BufWriter, Write},
};

extern crate voxell_timer;
use voxell_timer::time_fn;

use anyhow::{Context, Result};

trait QuickStringify {
    fn qs(self) -> String;
}

impl QuickStringify for &[u8] {
    fn qs(self) -> String {
        String::from_utf8(self.to_vec()).context("converting u8 slice to string").unwrap()
    }
}

macro_rules! qs_trivial_impl {
    ($($t:ty)*) => {
        $(
            impl QuickStringify for $t {
                fn qs(self) -> String {
                    self.to_string()
                }
            }
        )*
    };

    ($t:ty) => {
        impl QuickStringify for $t {
            fn qs(self) -> String {
                self.to_string()
            }
        }
    };

}

qs_trivial_impl!(String & str);
qs_trivial_impl!(u8 u16 u32 u64 u128 usize i8 i16 i32 i64 i128 isize);
qs_trivial_impl!(f32 f64);
qs_trivial_impl!(Section);

macro_rules! write_test_fn_for_filename {
    ($fn_name:ident, $in_path:literal, $out_path:literal) => {
        #[test]
        pub fn $fn_name() {
            const BEATMAP: &str = include_str!($in_path);
            let parser = BeatmapParser::new(BEATMAP, ParserMode::Verbose);

            let (items, duration) = time_fn(|| parser.parse_debug());

            let len = items.len();
            let res: Result<()> = try {
                let f = File::create($out_path).context("creating test output file")?;
                let mut writer = BufWriter::new(f);
                for item in items {
                    let DebugItem { source: src, item } = item;
                    match item {
                        Item::Literal(lit) => {
                            writer
                                .write_all(format!("{:?} => Literal({:?})\n", src.qs(), lit.qs()).as_bytes())
                                .context("writing to file")?;
                        }

                        Item::Integer(lit) => {
                            writer
                                .write_all(format!("{:?} => Integer({:?}\n", src.qs(), lit.qs()).as_bytes())
                                .context("writing to file")?;
                        }

                        Item::Float(lit) => {
                            writer
                                .write_all(format!("{:?} => Float({:?})\n", src.qs(), lit.qs()).as_bytes())
                                .context("writing to file")?;
                        }

                        Item::QuotedString(lit) => {
                            writer
                                .write_all(format!("{:?} => QuotedString({:?})\n", src.qs(), lit.qs()).as_bytes())
                                .context("writing to file")?;
                        }

                        Item::ValueUntilNewline(lit) => {
                            writer
                                .write_all(format!("{:?} => ValueUntilNewline({:?})\n", src.qs(), lit.qs()).as_bytes())
                                .context("writing to file")?;
                        }

                        Item::SectionHeader(lit) => {
                            writer
                                .write_all(format!("{:?} => SectionHeader({:?})\n", src.qs(), lit.qs()).as_bytes())
                                .context("writing to file")?;
                        }

                        Item::Newline => {
                            writer
                                .write_all(format!("{:?} => Newline\n", src.qs()).as_bytes())
                                .context("writing to file")?;
                        }

                        Item::FileFormatMagic => {
                            writer
                                .write_all(format!("{:?} => FileFormatMagic\n", src.qs()).as_bytes())
                                .context("writing to file")?;
                        }

                        _ => {
                            writer
                                .write_all(format!("{:?} => {:?}\n", src.qs(), item).as_bytes())
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

            let result_string = format!("Parsing completed in {:?}\n\t{} items parsed", duration, len);

            panic!("{}", result_string);
        }
    };
}

write_test_fn_for_filename!(test_beatmap_1, r"..\tests\testbeatmap1.osu", r".\tests\testbeatmap1.osu.out");
write_test_fn_for_filename!(test_beatmap_2, r"..\tests\testbeatmap2.osu", r".\tests\testbeatmap2.osu.out");
write_test_fn_for_filename!(test_beatmap_3, r"..\tests\testbeatmap3.osu", r".\tests\testbeatmap3.osu.out");
