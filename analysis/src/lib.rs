#![allow(dead_code)] // TODO: no
#![warn(rust_2018_idioms)]

mod ctxt;
pub mod ir;
mod lower;
pub mod ty;

pub use ctxt::LoweringCx;
pub use lower::lower_translation_unit;
use parser::Span;

#[derive(Debug)]
pub struct Error {
    msg: String,
    span: Option<Span>,
    notes: Vec<Note>,
}

#[derive(Debug)]
struct Note {
    msg: String,
    span: Option<Span>,
}

impl Error {
    pub fn new(msg: impl Into<String>, span: Span) -> Self {
        Self {
            msg: msg.into(),
            span: Some(span),
            notes: Vec::new(),
        }
    }

    pub fn new_without_span(msg: impl Into<String>) -> Self {
        Self {
            msg: msg.into(),
            span: None,
            notes: Vec::new(),
        }
    }

    pub fn note_spanned(mut self, msg: impl Into<String>, span: Span) -> Self {
        self.notes.push(Note {
            msg: msg.into(),
            span: Some(span),
        });
        self
    }
}
