#![allow(dead_code)] // TODO: no
#![warn(rust_2018_idioms)]

use std::fmt::Debug;

use ast::TranslationUnit;
use dbg_pls::DebugPls;

use crate::token::Token;

pub mod ast;
mod parser;
mod pre;
pub mod pretty;
mod sym;
mod token;

pub use sym::Symbol;

pub use crate::parser::Parser;

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

    pub fn dummy() -> Self {
        Self::start_end(0, 0)
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

#[derive(Debug)]
pub struct Error {
    pub msg: String,
    pub span: Option<Span>,
    pub notes: Vec<Note>,
}

#[derive(Debug)]
pub struct Note {
    pub msg: String,
    pub span: Option<Span>,
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

impl DebugPls for Error {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_struct("Error")
            .field("span", &self.span)
            .field("msg", &self.msg)
            .finish();
    }
}

fn lex_and_pre(src: &str) -> impl Iterator<Item = (Token<'_>, Span)> + '_ {
    let pre_tokens = pre::preprocess_tokens(src);
    token::pre_tokens_to_tokens(pre_tokens)
}

pub fn parse_file(src: &str) -> Result<TranslationUnit, Error> {
    let lexer = lex_and_pre(src);
    parser::parse_declarations(lexer)
}
