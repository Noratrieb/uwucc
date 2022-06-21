//! Contrary to popular belief, Dennis Ritchie did not invent the C grammar.
//! The C grammar was brought to Dennis Ritchie by a demon in his worst dreams

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
    Comma,
    /// #   %:
    Hash,
    /// ##   %:%:
    HashHash,
}

struct PLexer<I>
where
    I: Iterator<Item = u8>,
{
    src: peekmore::PeekMoreIterator<I>,
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

    fn s_p(&mut self) -> Option<&u8> {
        self.src.peek()
    }

    fn s_p_n(&mut self, n: usize) -> Option<&u8> {
        self.src.peek_nth(n)
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
        use PToken::Punctuator as TokP;

        loop {
            match self.src.next()? {
                c @ (b'a'..=b'z' | b'A'..=b'Z' | b'_') => {
                    return Some(self.identifier(c));
                }
                c @ b'0'..=b'9' => return Some(self.number(c)),
                b'"' => return Some(self.string_literal()),
                c if c.is_ascii_whitespace() => {}
                b'/' if self.s_p() == Some(&b'*') => loop {
                    let first = self.src.next()?;
                    let second = self.s_p();
                    if first == b'*' && second == Some(&b'/') {
                        self.src.next();
                    }
                },
                b'/' if self.s_p() == Some(&b'/') => while self.src.next() != Some(b'\n') {},
                b'.' if self.s_p() == Some(&b'.') && self.s_p_n(2) == Some(&b'.') => {
                    self.src.next();
                    self.src.next();
                    return Some(TokP(Punctuator::LeftLeftChevronEqual));
                }
                b'<' if self.s_p() == Some(&b'<') && self.s_p_n(2) == Some(&b'=') => {
                    self.src.next();
                    self.src.next();
                    return Some(TokP(Punctuator::LeftLeftChevronEqual));
                }
                b'>' if self.s_p() == Some(&b'>') && self.s_p_n(2) == Some(&b'=') => {
                    self.src.next();
                    self.src.next();
                    return Some(TokP(Punctuator::RightRightChevronEqual));
                }
                b'<' if self.s_p() == Some(&b':') => {
                    self.src.next();
                    return Some(TokP(Punctuator::BracketOpen));
                }
                b':' if self.s_p() == Some(&b'>') => {
                    self.src.next();
                    return Some(TokP(Punctuator::BracketClose));
                }
                b'<' if self.s_p() == Some(&b'%') => {
                    self.src.next();
                    return Some(TokP(Punctuator::BraceOpen));
                }
                b'%' if self.s_p() == Some(&b'>') => {
                    self.src.next();
                    return Some(TokP(Punctuator::BraceClose));
                }
                b'-' if self.s_p() == Some(&b'>') => {
                    self.src.next();
                    return Some(TokP(Punctuator::Arrow));
                }
                b'+' if self.s_p() == Some(&b'+') => {
                    self.src.next();
                    return Some(TokP(Punctuator::PlusPlus));
                }
                b'-' if self.s_p() == Some(&b'-') => {
                    self.src.next();
                    return Some(TokP(Punctuator::Minus));
                }
                b'<' if self.s_p() == Some(&b'<') => {
                    self.src.next();
                    return Some(TokP(Punctuator::LeftLeftChevron));
                }
                b'>' if self.s_p() == Some(&b'>') => {
                    self.src.next();
                    return Some(TokP(Punctuator::RightRightChevron));
                }
                b'<' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::LeftChevronEqual));
                }
                b'>' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::RightChevronEqual));
                }
                b'=' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::EqualEqual));
                }
                b'!' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::BangEqual));
                }
                b'&' if self.s_p() == Some(&b'&') => {
                    self.src.next();
                    return Some(TokP(Punctuator::AmpersandAmpersand));
                }
                b'|' if self.s_p() == Some(&b'|') => {
                    self.src.next();
                    return Some(TokP(Punctuator::PipePipe));
                }
                b'*' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::AsteriskEqual));
                }
                b'/' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::SlashEqual));
                }
                b'%' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::PercentEqual));
                }
                b'+' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::PlusEqual));
                }
                b'-' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::MinusEqual));
                }
                b'&' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::AmspersandEqual));
                }
                b'^' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::CaretEqual));
                }
                b'|' if self.s_p() == Some(&b'=') => {
                    self.src.next();
                    return Some(TokP(Punctuator::PipeEqual));
                }
                b'[' => return Some(TokP(Punctuator::BracketOpen)),
                b']' => return Some(TokP(Punctuator::BracketClose)),
                b'(' => return Some(TokP(Punctuator::ParenOpen)),
                b'{' => return Some(TokP(Punctuator::BraceOpen)),
                b')' => return Some(TokP(Punctuator::ParenClose)),
                b'}' => return Some(TokP(Punctuator::BraceClose)),
                b'.' => return Some(TokP(Punctuator::Dot)),
                b'&' => return Some(TokP(Punctuator::Ampersand)),
                b'*' => return Some(TokP(Punctuator::Asterisk)),
                b'-' => return Some(TokP(Punctuator::Minus)),
                b'~' => return Some(TokP(Punctuator::Tilde)),
                b'!' => return Some(TokP(Punctuator::Bang)),
                b'%' => return Some(TokP(Punctuator::Percent)),
                b'<' => return Some(TokP(Punctuator::LeftChevron)),
                b'>' => return Some(TokP(Punctuator::RightChevron)),
                b'^' => return Some(TokP(Punctuator::Caret)),
                b'|' => return Some(TokP(Punctuator::Pipe)),
                b'?' => return Some(TokP(Punctuator::QuestionMark)),
                b':' => return Some(TokP(Punctuator::Colon)),
                b';' => return Some(TokP(Punctuator::Semicolon)),
                b'=' => return Some(TokP(Punctuator::Equal)),
                b',' => return Some(TokP(Punctuator::Comma)),
                b'#' => return Some(TokP(Punctuator::Hash)),
                c => return Some(PToken::OtherNonWs(c)),
            }
        }
    }
}
