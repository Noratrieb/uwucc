use crate::pre::Punctuator;

pub enum Token {
    Keyword(Keyword),
    Identifier(),
    Constant(),
    StringLiteral(),
    Punctuator(Punctuator),
    Error,
}

pub struct Keyword;
