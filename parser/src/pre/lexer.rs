//! Contrary to popular belief, Dennis Ritchie did not invent the C grammar.
//! The C grammar was brought to Dennis Ritchie by a demon in his worst dreams
//!
//! Code might be bad. Possibly.

use std::ops::Not;

use peekmore::PeekMore;

use crate::Span;

#[derive(Debug)]
pub enum PToken {
    HeaderName(Vec<u8>),
    Identifier(Vec<u8>),
    PpNumber(Vec<u8>),
    CharConstant,
    StringLiteral(Vec<u8>),
    Punctuator(Punctuator),
    OtherNonWs(u8),
    Error,
}

#[derive(Debug)]
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

struct PLexer<I>
where
    I: Iterator<Item = (usize, u8)>,
{
    src: peekmore::PeekMoreIterator<I>,
}

impl<I> PLexer<I>
where
    I: Iterator<Item = (usize, u8)>,
{
    /// 6.4.2 Identifiers
    /// TODO: 6.4.3 Universal character names
    fn identifier(&mut self, c: u8, mut last_span: usize) -> (PToken, usize) {
        let mut ident = vec![c];

        while let Some((span, c)) = self.src.peek() {
            let (span, c) = (*span, *c);
            if c.is_c_identifier() {
                self.src.next();
                ident.push(c);
                last_span = span;
            } else {
                break;
            }
        }

        (PToken::Identifier(ident), last_span)
    }

    /// 6.4.8 Preprocessing numbers
    fn number(&mut self, c: u8, mut last_span: usize) -> (PToken, usize) {
        let mut number = vec![c];

        while let Some((span, c)) = self.src.peek() {
            let (span, c) = (*span, *c);
            if c.is_ascii_digit() {
                self.src.next();
                number.push(c);
                last_span = span;
            } else {
                break;
            }
        }

        (PToken::PpNumber(number), last_span)
    }

    /// 6.4.5 String literals
    fn string_literal(&mut self, mut last_span: usize) -> (PToken, usize) {
        let mut string = Vec::new();

        loop {
            let next = self.src.next();
            match next {
                Some((span, c)) => {
                    if c == b'"' {
                        break;
                    }
                    last_span = span;
                    string.push(c);
                }
                None => return (PToken::Error, last_span),
            }
        }

        (PToken::StringLiteral(string), last_span)
    }

    /// source peek
    fn s_p(&mut self) -> Option<&(usize, u8)> {
        self.src.peek()
    }

    /// source peek nth
    fn s_p_n(&mut self, n: usize) -> Option<&(usize, u8)> {
        self.src.peek_nth(n)
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

impl<'src, I> Iterator for PLexer<I>
where
    I: Iterator<Item = (usize, u8)>,
{
    type Item = (PToken, Span);

    /// preprocessing-token:
    ///   header-name
    ///   identifier
    ///   pp-number
    ///   character-constant
    ///   string-literal
    ///   punctuator
    ///   each non-white-space character that cannot be one of the above
    fn next(&mut self) -> Option<Self::Item> {
        use PToken::Punctuator as TokP;

        let (start_span, char1) = self.src.next()?;
        let char2 = self.src.peek().map(|(_, c)| *c);
        let char3 = self.src.peek_nth(2).map(|(_, c)| *c);

        let (token, end_span) = loop {
            match (char1, char2, char3) {
                // IDENTIFIER
                (c, _, _) if c.is_c_identifier_nondigit() => {
                    break self.identifier(c, start_span);
                }
                // NUMBER
                (c, _, _) if c.is_c_identifier_digit() => break self.number(c, start_span),
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

        Some((token, start_span..end_span + 1))
    }
}

pub fn preprocess_tokens(
    src: impl Iterator<Item = (usize, u8)>,
) -> impl Iterator<Item = (PToken, std::ops::Range<usize>)> {
    let lexer = PLexer {
        src: src.peekmore(),
    };
    lexer
}

#[cfg(test)]
mod tests {
    fn lex_test(str: &str) {
        let bytes = str.bytes().enumerate();
        let tokens = super::preprocess_tokens(bytes);
        let tokens = tokens.collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn hello_world() {
        let src = r#"\
int main() {
    puts("Hello, World!");
}
"#;
    lex_test(src);
    }
}
