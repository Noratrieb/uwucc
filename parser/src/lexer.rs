//! Contrary to popular belief, Dennis Ritchie did not invent the C grammar.
//! The C grammar was brought to Dennis Ritchie by a demon in hos worst dreams

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

pub enum Token {
    Keyword(Keyword),
    Identifier(),
    Constant(),
    StringLiteral(),
    Punctuator(Punctuator),
}

pub enum Keyword {}

pub enum Constant {
    Integer(i64),
}

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
    /// ~
    Tilde,
    /// ! 🔫
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
    LeftChevronEqual,
    /// >=
    RightChevronEqual,
    /// ==
    EqualEqual,
    /// !=
    BangEqual,
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
    Equal,
    /// *=
    AsteriskEqual,
    /// /=
    SlashEqual,
    /// %=
    PercentEqual,
    /// +=
    PlusEqual,
    /// -=
    MinusEqual,
    /// <<=
    LeftLeftChevronEqual,
    /// >>=
    RightRightChevronEqual,
    /// &=
    AmspersandEqual,
    /// ^=
    CaretEqual,
    /// |=
    PipeEqual,
    /// ,
    Comman,
    /// #   %:
    Hash,
    /// ##   %:%:
    HashHash,
}

struct PLexer<I>
where
    I: Iterator<Item = u8>,
{
    src: std::iter::Peekable<I>,
}

impl<I> PLexer<I>
where
    I: Iterator<Item = u8>,
{
    /// 6.4.2 Identifiers
    /// TODO: 6.4.3 Universal character names
    fn identifier(&mut self, c: u8) -> PToken {
        let mut ident = vec![c];

        while let Some(&c) = self.src.peek() {
            if let b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9' = c {
                self.src.next();
                ident.push(c);
            } else {
                break;
            }
        }

        PToken::Identifier(ident)
    }

    /// 6.4.8 Preprocessing numbers
    fn number(&mut self, c: u8) -> PToken {
        let mut number = vec![c];

        while let Some(&c) = self.src.peek() {
            if let b'0'..=b'9' = c {
                self.src.next();
                number.push(c);
            } else {
                break;
            }
        }

        PToken::PpNumber(number)
    }

    /// 6.4.5 String literals
    fn string_literal(&mut self) -> PToken {
        let mut string = Vec::new();

        while let c @ b'"' = {
            match self.src.next() {
                Some(next) => next,
                None => return PToken::Error,
            }
        } {
            string.push(c);
        }
        PToken::StringLiteral(string)
    }
}

impl<'src, I> Iterator for PLexer<I>
where
    I: Iterator<Item = u8>,
{
    type Item = PToken;

    /// preprocessing-token:
    ///   header-name
    ///   identifier
    ///   pp-number
    ///   character-constant
    ///   string-literal
    ///   punctuator
    ///   each non-white-space character that cannot be one of the above
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.src.next()? {
                c @ (b'a'..=b'z' | b'A'..=b'Z' | b'_') => {
                    return Some(self.identifier(c));
                }
                c @ b'0'..=b'9' => return Some(self.number(c)),
                b'"' => return Some(self.string_literal()),
                b'[' => return Some(PToken::Punctuator(Punctuator::BraceOpen)),
                c if c.is_ascii_whitespace() => {}
                c => return Some(PToken::OtherNonWs(c)),
            }
        }
    }
}
