//! Contrary to popular belief, Dennis Ritchie did not invent the C grammar.
//! The C grammar was brought to Dennis Ritchie by a demon in his worst dreams
//!
//! Code might be bad. Possibly.

use std::{fmt::Display, ops::Not};

use peekmore::PeekMore;

use crate::Span;

#[derive(Debug, Clone, Copy)]
pub enum PToken<'src> {
    HeaderName(&'src str),
    Identifier(&'src str),
    PpNumber(&'src str),
    CharConstant(u8),
    StringLiteral(&'src str),
    Punctuator(Punctuator),
    OtherNonWs(u8),
    Error,
}

#[derive(Debug, Clone, Copy)]
pub enum Punctuator {
    /// [   <:
    BracketOpen,
    /// ]   :>
    BracketClose,
    /// (
    ParenOpen,
    /// )
    ParenClose,
    /// {   <%
    BraceOpen,
    /// }   %>
    BraceClose,
    /// .
    Dot,
    /// ->
    Arrow,
    /// ++
    PlusPlus,
    /// --
    MinusMinus,
    /// &
    Ampersand,
    /// *
    Asterisk,
    /// +
    Plus,
    /// -
    Minus,
    /// ~ (squiggly)
    Tilde,
    /// ! 🤯
    Bang,
    /// /
    Slash,
    //// %
    Percent,
    /// <<
    LeftLeftChevron,
    /// >>
    RightRightChevron,
    /// <
    LeftChevron,
    /// >
    RightChevron,
    /// <=
    LeftChevronEq,
    /// >=
    RightChevronEq,
    /// ==
    EqEq,
    /// !=
    BangEq,
    /// ^
    Caret,
    /// |
    Pipe,
    /// &&
    AmpersandAmpersand,
    /// ||
    PipePipe,
    /// ?
    QuestionMark,
    /// :
    Colon,
    /// ;
    Semicolon,
    /// ...
    DotDotDot,
    /// =
    Eq,
    /// *=
    AsteriskEq,
    /// /=
    SlashEq,
    /// %=
    PercentEq,
    /// +=
    PlusEq,
    /// -=
    MinusEq,
    /// <<=
    LeftLeftChevronEq,
    /// >>=
    RightRightChevronEq,
    /// &=
    AmspersandEq,
    /// ^=
    CaretEq,
    /// |=
    PipeEq,
    /// ,
    Comma,
    /// #   %:
    Hash,
    /// ##   %:%:
    HashHash,
}

impl Display for Punctuator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Punctuator::BracketOpen => f.write_str("["),
            Punctuator::BracketClose => f.write_str("]"),
            Punctuator::ParenOpen => f.write_str("("),
            Punctuator::ParenClose => f.write_str(")"),
            Punctuator::BraceOpen => f.write_str("{"),
            Punctuator::BraceClose => f.write_str("}"),
            Punctuator::Dot => f.write_str("."),
            Punctuator::Arrow => f.write_str("->"),
            Punctuator::PlusPlus => f.write_str("++"),
            Punctuator::MinusMinus => f.write_str("--"),
            Punctuator::Ampersand => f.write_str("&"),
            Punctuator::Asterisk => f.write_str("*"),
            Punctuator::Plus => f.write_str("+"),
            Punctuator::Minus => f.write_str("-"),
            Punctuator::Tilde => f.write_str("~"),
            Punctuator::Bang => f.write_str("!"),
            Punctuator::Slash => f.write_str("/"),
            Punctuator::Percent => f.write_str("%"),
            Punctuator::LeftLeftChevron => f.write_str("<<"),
            Punctuator::RightRightChevron => f.write_str(">>"),
            Punctuator::LeftChevron => f.write_str("<"),
            Punctuator::RightChevron => f.write_str(">"),
            Punctuator::LeftChevronEq => f.write_str("<="),
            Punctuator::RightChevronEq => f.write_str(">="),
            Punctuator::EqEq => f.write_str("=="),
            Punctuator::BangEq => f.write_str("!="),
            Punctuator::Caret => f.write_str("^"),
            Punctuator::Pipe => f.write_str("|"),
            Punctuator::AmpersandAmpersand => f.write_str("&&"),
            Punctuator::PipePipe => f.write_str("||"),
            Punctuator::QuestionMark => f.write_str("?"),
            Punctuator::Colon => f.write_str(":"),
            Punctuator::Semicolon => f.write_str(";"),
            Punctuator::DotDotDot => f.write_str("..."),
            Punctuator::Eq => f.write_str("="),
            Punctuator::AsteriskEq => f.write_str("*="),
            Punctuator::SlashEq => f.write_str("/="),
            Punctuator::PercentEq => f.write_str("%="),
            Punctuator::PlusEq => f.write_str("+="),
            Punctuator::MinusEq => f.write_str("-="),
            Punctuator::LeftLeftChevronEq => f.write_str("<<="),
            Punctuator::RightRightChevronEq => f.write_str(">>="),
            Punctuator::AmspersandEq => f.write_str("&="),
            Punctuator::CaretEq => f.write_str("^="),
            Punctuator::PipeEq => f.write_str("|="),
            Punctuator::Comma => f.write_str(","),
            Punctuator::Hash => f.write_str("#"),
            Punctuator::HashHash => f.write_str("##"),
        }
    }
}

struct PLexer<'src, I>
where
    I: Iterator<Item = (usize, u8)>,
{
    src_str: &'src str,
    src: peekmore::PeekMoreIterator<I>,
}

impl<'src, I> PLexer<'src, I>
where
    I: Iterator<Item = (usize, u8)>,
{
    /// 6.4.2 Identifiers
    /// TODO: 6.4.3 Universal character names
    fn identifier(&mut self, mut last_span: usize) -> (PToken<'src>, usize) {
        let first_span = last_span;

        while let Some((span, c)) = self.src.peek() {
            let (span, c) = (*span, *c);
            if c.is_c_identifier() {
                self.src.next();
                last_span = span;
            } else {
                break;
            }
        }

        (
            PToken::Identifier(&self.src_str[first_span..=last_span]),
            last_span,
        )
    }

    /// 6.4.8 Preprocessing numbers
    fn number(&mut self, mut last_span: usize) -> (PToken<'src>, usize) {
        let first_span = last_span;

        while let Some((span, c)) = self.src.peek() {
            let (span, c) = (*span, *c);
            if c.is_ascii_digit() {
                self.src.next();
                last_span = span;
            } else {
                break;
            }
        }

        (
            PToken::PpNumber(&self.src_str[first_span..=last_span]),
            last_span,
        )
    }

    /// 6.4.5 String literals
    fn string_literal(&mut self, mut last_span: usize) -> (PToken<'src>, usize) {
        let first_span = last_span;

        loop {
            let next = self.src.next();
            match next {
                Some((span, c)) => {
                    if c == b'"' {
                        break;
                    }
                    last_span = span;
                }
                None => return (PToken::Error, last_span),
            }
        }

        (
            PToken::StringLiteral(&self.src_str[first_span + 1..=last_span]),
            last_span,
        )
    }

    /// source peek
    fn s_p(&mut self) -> Option<&(usize, u8)> {
        self.src.peek()
    }
}

macro_rules! triple_punct {
    ($self:ident, $tok:ident) => {{
        $self.src.next()?;
        let (end, _) = $self.src.next()?;
        break (TokP(Punctuator::$tok), end);
    }};
}

macro_rules! double_punct {
    ($self:ident, $tok:ident) => {{
        let (end, _) = $self.src.next()?;
        break (TokP(Punctuator::$tok), end);
    }};
}

trait CLexExt {
    fn is_c_identifier_nondigit(&self) -> bool;

    fn is_c_identifier_digit(&self) -> bool;

    fn is_c_identifier(&self) -> bool {
        self.is_c_identifier_nondigit() || self.is_c_identifier_digit()
    }

    fn is_c_whitespace(&self) -> bool;
}

impl CLexExt for u8 {
    fn is_c_identifier_nondigit(&self) -> bool {
        matches!(self, b'a'..=b'z' | b'A'..=b'Z' | b'_')
    }

    fn is_c_identifier_digit(&self) -> bool {
        matches!(self, b'0'..=b'9')
    }

    fn is_c_whitespace(&self) -> bool {
        self.is_ascii_whitespace() // TODO: is it?
    }
}

impl<'src, I> Iterator for PLexer<'src, I>
where
    I: Iterator<Item = (usize, u8)>,
{
    type Item = (PToken<'src>, Span);

    /// preprocessing-token:
    ///   header-name         TODO
    ///   identifier
    ///   pp-number
    ///   character-constant  TODO
    ///   string-literal
    ///   punctuator
    ///   each non-white-space character that cannot be one of the above
    fn next(&mut self) -> Option<Self::Item> {
        use PToken::Punctuator as TokP;

        let mut start_span;

        let (token, end_span) = loop {
            let (span, char1) = self.src.next()?;
            start_span = span;
            let char2 = self.src.peek().map(|(_, c)| *c);
            let char3 = self.src.peek_nth(1).map(|(_, c)| *c);

            match (char1, char2, char3) {
                // IDENTIFIER
                (c, _, _) if c.is_c_identifier_nondigit() => {
                    break self.identifier(start_span);
                }
                // NUMBER
                (c, _, _) if c.is_c_identifier_digit() => break self.number(start_span),
                // STRING
                (b'"', _, _) => break self.string_literal(start_span),
                // WHITESPACE
                (c, _, _) if c.is_c_whitespace() => {}
                // COMMENTS
                (b'/', Some(b'*'), _) => loop {
                    let (_, first) = self.src.next()?;
                    let second = self.s_p().map(|(_, c)| *c);
                    if first == b'*' && second == Some(b'/') {
                        self.src.next();
                        break;
                    }
                },
                (b'/', Some(b'/'), _) => while matches!(self.src.next(), Some((_, b'\n'))).not() {},
                // TRIPLE CHARACTER PUNCTUATION
                (b'.', Some(b'.'), Some(b'.')) => triple_punct!(self, DotDotDot),
                (b'<', Some(b'<'), Some(b'=')) => triple_punct!(self, LeftLeftChevronEq),
                (b'>', Some(b'>'), Some(b'=')) => triple_punct!(self, RightRightChevronEq),
                // DOUBLE CHARACTER PUNCTUATION
                (b'<', Some(b':'), _) => double_punct!(self, BracketOpen),
                (b':', Some(b'>'), _) => double_punct!(self, BracketClose),
                (b'<', Some(b'%'), _) => double_punct!(self, BraceOpen),
                (b'%', Some(b'>'), _) => double_punct!(self, BraceClose),
                (b'-', Some(b'>'), _) => double_punct!(self, Arrow),
                (b'+', Some(b'+'), _) => double_punct!(self, PlusPlus),
                (b'-', Some(b'-'), _) => double_punct!(self, MinusMinus),
                (b'<', Some(b'<'), _) => double_punct!(self, LeftLeftChevron),
                (b'>', Some(b'>'), _) => double_punct!(self, RightRightChevron),
                (b'<', Some(b'='), _) => double_punct!(self, PlusPlus),
                (b'>', Some(b'='), _) => double_punct!(self, RightChevronEq),
                (b'=', Some(b'='), _) => double_punct!(self, EqEq),
                (b'!', Some(b'='), _) => double_punct!(self, BangEq),
                (b'&', Some(b'&'), _) => double_punct!(self, AmpersandAmpersand),
                (b'|', Some(b'|'), _) => double_punct!(self, PipePipe),
                (b'*', Some(b'='), _) => double_punct!(self, AsteriskEq),
                (b'/', Some(b'='), _) => double_punct!(self, SlashEq),
                (b'%', Some(b'='), _) => double_punct!(self, PercentEq),
                (b'+', Some(b'='), _) => double_punct!(self, PlusEq),
                (b'-', Some(b'='), _) => double_punct!(self, MinusEq),
                (b'&', Some(b'='), _) => double_punct!(self, AmspersandEq),
                (b'^', Some(b'='), _) => double_punct!(self, CaretEq),
                (b'|', Some(b'='), _) => double_punct!(self, PipeEq),
                // SINGLE CHARACTER PUNCTUATION
                (b'[', _, _) => break (TokP(Punctuator::BracketOpen), start_span),
                (b']', _, _) => break (TokP(Punctuator::BracketClose), start_span),
                (b'(', _, _) => break (TokP(Punctuator::ParenOpen), start_span),
                (b'{', _, _) => break (TokP(Punctuator::BraceOpen), start_span),
                (b')', _, _) => break (TokP(Punctuator::ParenClose), start_span),
                (b'}', _, _) => break (TokP(Punctuator::BraceClose), start_span),
                (b'.', _, _) => break (TokP(Punctuator::Dot), start_span),
                (b'&', _, _) => break (TokP(Punctuator::Ampersand), start_span),
                (b'*', _, _) => break (TokP(Punctuator::Asterisk), start_span),
                (b'-', _, _) => break (TokP(Punctuator::Minus), start_span),
                (b'~', _, _) => break (TokP(Punctuator::Tilde), start_span),
                (b'!', _, _) => break (TokP(Punctuator::Bang), start_span),
                (b'/', _, _) => break (TokP(Punctuator::Slash), start_span),
                (b'%', _, _) => break (TokP(Punctuator::Percent), start_span),
                (b'<', _, _) => break (TokP(Punctuator::LeftChevron), start_span),
                (b'>', _, _) => break (TokP(Punctuator::RightChevron), start_span),
                (b'^', _, _) => break (TokP(Punctuator::Caret), start_span),
                (b'|', _, _) => break (TokP(Punctuator::Pipe), start_span),
                (b'?', _, _) => break (TokP(Punctuator::QuestionMark), start_span),
                (b':', _, _) => break (TokP(Punctuator::Colon), start_span),
                (b';', _, _) => break (TokP(Punctuator::Semicolon), start_span),
                (b'=', _, _) => break (TokP(Punctuator::Eq), start_span),
                (b',', _, _) => break (TokP(Punctuator::Comma), start_span),
                (b'#', _, _) => break (TokP(Punctuator::Hash), start_span),
                (c, _, _) => break (PToken::OtherNonWs(c), start_span),
            }
        };

        Some((token, Span::start_end(start_span, end_span + 1)))
    }
}

pub fn preprocess_tokens(src: &str) -> impl Iterator<Item = (PToken<'_>, Span)> {
    let lexer = PLexer {
        src_str: src,
        src: src.bytes().enumerate().peekmore(),
    };
    lexer
}

#[cfg(test)]
mod tests {
    macro_rules! lex_test {
        ($str:expr) => {
            let tokens = super::preprocess_tokens($str);
            let tokens = tokens.collect::<Vec<_>>();
            insta::assert_debug_snapshot!(tokens);
        };
    }

    #[test]
    fn identifiers() {
        let src = r#"AAAA BBBB CCCC"#;
        lex_test!(src);
    }

    #[test]
    fn left_left_chevron_eq() {
        let src = r#". <<= ."#;
        lex_test!(src);
    }

    #[test]
    fn hello_world() {
        let src = r#"
int main() {
    puts("Hello, World!");
}
"#;
        lex_test!(src);
    }

    #[test]
    fn some_operators() {
        let src = r#"
int hello(const char* uwu) <%
    uwu[5] <<= 23;
    *uwu * (p++);
    return p;
%>"#;

        lex_test!(src);
    }
}
