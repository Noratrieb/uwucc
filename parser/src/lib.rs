#![allow(dead_code)]
#![warn(rust_2018_idioms)]

mod ast;
mod parser;
mod pre;
mod token;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn start_end(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn extend(&self, rhs: Self) -> Self {
        Self::start_end(self.start, rhs.end)
    }
}

impl dbg_pls::DebugPls for Span {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {}
}

pub fn parse_file(src: &str) {
    println!("{src}");
}
