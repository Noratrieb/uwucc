#![allow(dead_code)] // TODO: no
#![warn(rust_2018_idioms)]

mod ir;
mod lower;
mod ty;

pub use lower::lower_translation_unit;
use parser::Span;

pub struct Error {
    msg: String,
    span: Span,
}

impl Error {
    pub fn new(msg: impl Into<String>, span: Span) -> Self {
        Self {
            msg: msg.into(),
            span,
        }
    }
}
