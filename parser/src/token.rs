use crate::{
    pre::{PToken, Punctuator},
    Span,
};

pub enum Token {
    Keyword(Keyword),
    Identifier(),
    Constant(),
    StringLiteral(),
    Punctuator(Punctuator),
    Error,
}

pub struct Keyword;

fn from_pre_toks(
    pre_toks: impl Iterator<Item = (PToken, Span)>,
) -> impl IntoIterator<Item = (Token, Span)> {
    pre_toks.map(|token| todo!())
}
