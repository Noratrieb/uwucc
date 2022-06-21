#![allow(dead_code)]
#![warn(rust_2018_idioms)]

mod pre;
mod token;

pub type Span = std::ops::Range<usize>;

pub fn parse_file(src: &str) {
    println!("{src}");
}
