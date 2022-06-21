use peekmore::PeekMoreIterator;

use crate::{
    ast::{Spanned, TypeSpecifier},
    token::{Keyword as Kw, Token as Tok},
    Span,
};

struct ParserError {
    span: Span,
    message: String,
}

impl ParserError {
    fn new(span: Span, message: String) -> Self {
        Self { span, message }
    }

    fn eof() -> Self {
        Self::new(0..0, "unexpected end of file".to_string())
    }
}

type Result<T, E = ParserError> = std::result::Result<T, E>;

struct Parser<'src, I>
where
    I: Iterator<Item = (Tok<'src>, Span)>,
{
    lex: PeekMoreIterator<I>,
}

impl<'src, I> Parser<'src, I>
where
    I: Iterator<Item = (Tok<'src>, Span)>,
{
    fn type_specifier(&mut self) -> Result<Spanned<TypeSpecifier>> {
        let (token, span) = self.next_t()?;
        let ty = match token {
            Tok::Kw(Kw::Void) => TypeSpecifier::Void,
            Tok::Kw(Kw::Char) => TypeSpecifier::Char,
            Tok::Kw(Kw::Short) => TypeSpecifier::Short,
            Tok::Kw(Kw::Int) => TypeSpecifier::Int,
            Tok::Kw(Kw::Long) => TypeSpecifier::Long,
            Tok::Kw(Kw::Float) => TypeSpecifier::Float,
            Tok::Kw(Kw::Double) => TypeSpecifier::Double,
            Tok::Kw(Kw::Signed) => TypeSpecifier::Signed,
            Tok::Kw(Kw::Unsigned) => TypeSpecifier::Unsigned,
            Tok::Kw(Kw::Bool) => TypeSpecifier::Bool,
            Tok::Kw(Kw::Complex) => return Err(ParserError::new(span, "tf are you doing with complex numbers".to_string())),
            tok => return Err(ParserError::new(span, format!("Invalid token: `{tok}`"))),
        };
        Ok((ty, span))
    }

    fn next_t(&mut self) -> Result<(Tok<'src>, Span)> {
        self.lex.next().ok_or_else(ParserError::eof)
    }
}
