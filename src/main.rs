#![feature(try_blocks)]
extern crate anyhow;

mod parser;
#[cfg(test)]
mod test;

// /*
//  * Each timing point influences a specified portion of the map, commonly called a "timing section".
//  * The .osu file format requires these to be sorted in chronological order.
//  *
//  * Timing point syntax: time,beatLength,meter,sampleSet,sampleIndex,volume,uninherited,effects
//  *
//  * time (Integer): Start time of the timing section, in milliseconds from the beginning of the beatmap's audio.
//  *     The end of the timing section is the next timing point's time (or never, if this is the last timing point).
//  * beatLength (f64): This property has two meanings:
//  *     For uninherited timing points, the duration of a beat, in milliseconds.
//  *     For inherited timing points, a negative inverse slider velocity multiplier, as a percentage. For example,
//  *         -50 would make all sliders in this timing section twice as fast as SliderMultiplier.
//  * meter (Integer): Amount of beats in a measure. Inherited timing points ignore this property.
//  * sampleSet (Integer): Default sample set for hit objects (0 = beatmap default, 1 = normal, 2 = soft, 3 = drum).
//  * sampleIndex (Integer): Custom sample index for hit objects. 0 indicates osu!'s default hitsounds.
//  * volume (Integer): Volume percentage for hit objects.
//  * uninherited (0 or 1): Whether or not the timing point is uninherited.
//  * effects (Integer): Bit flags that give the timing point extra effects. See the effects section.
//  */
// pub struct TimingPoint {
//     time: isize,
//     beat_length: f64,
//     inherited: TimingPointType,
// }

// pub enum TimingPointType {
//     Uninherited,
//     Inherited,
// }

fn main() {
    println!("Hello, world!");
}
