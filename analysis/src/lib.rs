#![allow(dead_code)] // TODO: no
#![warn(rust_2018_idioms)]

mod ctxt;
mod ir;
mod lower;
mod ty;

pub use lower::lower_translation_unit;
use parser::Span;

#[derive(Debug)]
pub struct Error {
    msg: String,
    span: Span,
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
            span,
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
