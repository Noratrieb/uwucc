use std::fmt::Display;

use crate::{
    pre::{PToken, Punctuator},
    Span,
};

/// token:
///   keyword
///   identifier
///   constant
///   string-literal
///   punctuator
#[derive(Debug, Clone, Copy)]
pub enum Token<'src> {
    Kw(Keyword),
    Ident(&'src str),
    Constant(Constant),
    StringLiteral(&'src str),
    Punct(Punctuator),
    Error,
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Alignas,
    Alignof,
    Atomic,
    Bool,
    Complex,
    Generic,
    Imaginary,
    Noreturn,
    StaticAssert,
    ThreadLocal,
}

#[derive(Debug, Clone, Copy)]
pub enum Constant {
    Int(i128),
    Float(f64),
    Char(u8),
    // adding enumerations here makes no sense.
}

fn ident_to_keyword(ident: &str) -> Option<Keyword> {
    match ident {
        "auto" => Some(Keyword::Auto),
        "break" => Some(Keyword::Break),
        "case" => Some(Keyword::Case),
        "char" => Some(Keyword::Char),
        "const" => Some(Keyword::Const),
        "continue" => Some(Keyword::Continue),
        "default" => Some(Keyword::Default),
        "do" => Some(Keyword::Do),
        "double" => Some(Keyword::Double),
        "else" => Some(Keyword::Else),
        "enum" => Some(Keyword::Enum),
        "extern" => Some(Keyword::Extern),
        "float" => Some(Keyword::Float),
        "for" => Some(Keyword::For),
        "goto" => Some(Keyword::Goto),
        "if" => Some(Keyword::If),
        "inline" => Some(Keyword::Inline),
        "int" => Some(Keyword::Int),
        "long" => Some(Keyword::Long),
        "register" => Some(Keyword::Register),
        "restrict" => Some(Keyword::Restrict),
        "return" => Some(Keyword::Return),
        "short" => Some(Keyword::Short),
        "signed" => Some(Keyword::Signed),
        "sizeof" => Some(Keyword::Sizeof),
        "static" => Some(Keyword::Static),
        "struct" => Some(Keyword::Struct),
        "switch" => Some(Keyword::Switch),
        "typedef" => Some(Keyword::Typedef),
        "union" => Some(Keyword::Union),
        "unsigned" => Some(Keyword::Unsigned),
        "void" => Some(Keyword::Void),
        "volatile" => Some(Keyword::Volatile),
        "while" => Some(Keyword::While),
        "_Alignas" => Some(Keyword::Alignas),
        "_Alignof" => Some(Keyword::Alignof),
        "_Atomic" => Some(Keyword::Atomic),
        "_Bool" => Some(Keyword::Bool),
        "_Complex" => Some(Keyword::Complex),
        "_Generic" => Some(Keyword::Generic),
        "_Imaginary" => Some(Keyword::Imaginary),
        "_Noreturn" => Some(Keyword::Noreturn),
        "_Static_assert" => Some(Keyword::StaticAssert),
        "_Thread_local" => Some(Keyword::ThreadLocal),
        _ => None,
    }
}

fn pp_number_to_constant(number: &str) -> Option<Constant> {
    let n = number.parse().ok()?;
    Some(Constant::Int(n))
}

pub fn pre_tokens_to_tokens<'src>(
    pre_tokens: impl Iterator<Item = (PToken<'src>, Span)>,
) -> impl Iterator<Item = (Token<'src>, Span)> {
    pre_tokens.map(|(token, span)| {
        let token = match token {
            PToken::HeaderName(_) => todo!("header names aren't real, wake up"),
            PToken::Identifier(ident) => match ident_to_keyword(ident) {
                Some(keyword) => Token::Kw(keyword),
                None => Token::Ident(ident),
            },
            PToken::PpNumber(number) => pp_number_to_constant(number)
                .map(Token::Constant)
                .unwrap_or(Token::Error),
            PToken::CharConstant(u8) => Token::Constant(Constant::Char(u8)),
            PToken::StringLiteral(lit) => Token::StringLiteral(lit),
            PToken::Punctuator(p) => Token::Punct(p),
            PToken::OtherNonWs(_) => Token::Error,
            PToken::Error => Token::Error,
        };
        (token, span)
    })
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Kw(kw) => Display::fmt(kw, f),
            Token::Ident(ident) => Display::fmt(ident, f),
            Token::Constant(c) => Display::fmt(c, f),
            Token::StringLiteral(str) => write!(f, "\"{}\"", str),
            Token::Punct(p) => Display::fmt(p, f),
            Token::Error => f.write_str("<invalid token>"),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Auto => f.write_str("auto"),
            Keyword::Break => f.write_str("break"),
            Keyword::Case => f.write_str("case"),
            Keyword::Char => f.write_str("char"),
            Keyword::Const => f.write_str("const"),
            Keyword::Continue => f.write_str("continue"),
            Keyword::Default => f.write_str("default"),
            Keyword::Do => f.write_str("do"),
            Keyword::Double => f.write_str("double"),
            Keyword::Else => f.write_str("else"),
            Keyword::Enum => f.write_str("enum"),
            Keyword::Extern => f.write_str("extern"),
            Keyword::Float => f.write_str("float"),
            Keyword::For => f.write_str("for"),
            Keyword::Goto => f.write_str("goto"),
            Keyword::If => f.write_str("if"),
            Keyword::Inline => f.write_str("inline"),
            Keyword::Int => f.write_str("int"),
            Keyword::Long => f.write_str("long"),
            Keyword::Register => f.write_str("register"),
            Keyword::Restrict => f.write_str("restrict"),
            Keyword::Return => f.write_str("return"),
            Keyword::Short => f.write_str("short"),
            Keyword::Signed => f.write_str("signed"),
            Keyword::Sizeof => f.write_str("sizeof"),
            Keyword::Static => f.write_str("static"),
            Keyword::Struct => f.write_str("struct"),
            Keyword::Switch => f.write_str("switch"),
            Keyword::Typedef => f.write_str("typedef"),
            Keyword::Union => f.write_str("union"),
            Keyword::Unsigned => f.write_str("unsigned"),
            Keyword::Void => f.write_str("void"),
            Keyword::Volatile => f.write_str("volatile"),
            Keyword::While => f.write_str("while"),
            Keyword::Alignas => f.write_str("_Alignas"),
            Keyword::Alignof => f.write_str("_Alignof"),
            Keyword::Atomic => f.write_str("_Atomic"),
            Keyword::Bool => f.write_str("_Bool"),
            Keyword::Complex => f.write_str("_Complex"),
            Keyword::Generic => f.write_str("_Generic"),
            Keyword::Imaginary => f.write_str("_Imaginary"),
            Keyword::Noreturn => f.write_str("_Noreturn"),
            Keyword::StaticAssert => f.write_str("_Static_assert"),
            Keyword::ThreadLocal => f.write_str("_Thread_local"),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Int(int) => Display::fmt(int, f),
            Constant::Float(float) => Display::fmt(float, f),
            Constant::Char(c) => write!(f, "'{}'", *c as char),
        }
    }
}

#[cfg(test)]
mod tests {
    macro_rules! lex_test {
        ($src:expr) => {
            let pre_tokens = crate::pre::preprocess_tokens($src);
            let tokens = super::pre_tokens_to_tokens(pre_tokens);
            let tokens = tokens.collect::<Vec<_>>();
            insta::assert_debug_snapshot!(tokens);
        };
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
}
