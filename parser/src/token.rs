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
#[derive(Debug)]
pub enum Token<'src> {
    Keyword(Keyword),
    Identifier(&'src str),
    Constant(Constant),
    StringLiteral(&'src str),
    Punctuator(Punctuator),
    Error,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
                Some(keyword) => Token::Keyword(keyword),
                None => Token::Identifier(ident),
            },
            PToken::PpNumber(number) => pp_number_to_constant(number)
                .map(Token::Constant)
                .unwrap_or(Token::Error),
            PToken::CharConstant(u8) => Token::Constant(Constant::Char(u8)),
            PToken::StringLiteral(lit) => Token::StringLiteral(lit),
            PToken::Punctuator(p) => Token::Punctuator(p),
            PToken::OtherNonWs(_) => Token::Error,
            PToken::Error => Token::Error,
        };
        (token, span)
    })
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