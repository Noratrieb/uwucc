#![allow(dead_code)]
#![warn(rust_2018_idioms)]

use std::fmt::Debug;

mod ast;
mod parser;
mod pre;
mod token;

pub type Spanned<T> = (T, Span);

#[derive(PartialEq, Eq, Clone, Copy, Default)]
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

    pub fn extend_option(&self, rhs: Option<Self>) -> Self {
        rhs.map(|s| self.extend(s)).unwrap_or(*self)
    }

    pub fn span_of_spanned_list<T>(list: &[Spanned<T>]) -> Option<Span> {
        list.iter().fold(None, |old_span, &(_, span)| {
            Some(old_span.map(|s| s.extend(span)).unwrap_or(span))
        })
    }
}

impl dbg_pls::DebugPls for Span {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        dbg_pls::DebugPls::fmt(&(self.start..self.end), f)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&(self.start..self.end), f)
    }
}

pub fn parse_file(src: &str) {
    println!("{src}");
}
