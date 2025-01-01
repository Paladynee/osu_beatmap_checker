#![feature(try_blocks)]

use voxell_timer::time_fn;

extern crate anyhow;

mod parser;
#[cfg(test)]
mod test;

fn main() {
    let arg1 = std::env::args().nth(1).expect("missing argument");
    let source = std::fs::read_to_string(arg1).expect("failed to read file");
    let parser = parser::BeatmapParser::new(&source, parser::ParserMode::Silent);
    let (items, time) = time_fn(|| parser.parse_debug());

    println!("src len: {}\n{} items parsed in {:?}", source.len(), items.len(), time);
}
