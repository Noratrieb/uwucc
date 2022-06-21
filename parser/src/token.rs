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
pub enum Token<'src> {
    Keyword(Keyword),
    Identifier(&'src str),
    Constant(Constant),
    StringLiteral(&'src str),
    Punctuator(Punctuator),
    Error,
}

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

pub enum Constant {}

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

pub fn pre_tokens_to_tokens<'src>(
    pre_tokens: impl Iterator<Item = (PToken<'src>, Span)>,
) -> impl Iterator<Item = (Token<'src>, Span)> {
    pre_tokens.map(|_| todo!())
}
