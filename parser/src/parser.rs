use peekmore::PeekMoreIterator;

use crate::{
    ast::{DeclAttr, DeclSpec, Spanned, TypeSpecifier},
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
        Self::new(Span::default(), "unexpected end of file".to_string())
    }

    fn unsupported(span: Span, token: &Tok<'_>) -> Self {
        Self::new(span, format!("`{token}` is not supported"))
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
    fn find_typedef(&self, ident: &str) -> Option<()> {
        None // TODO: this
    }

    fn next_t(&mut self) -> Result<(Tok<'src>, Span)> {
        self.lex.next().ok_or_else(ParserError::eof)
    }

    fn peek_t(&mut self) -> Result<&(Tok<'src>, Span)> {
        self.lex.peek().ok_or_else(ParserError::eof)
    }

    // -----------------------
    // Declarations
    // -----------------------

    /// (6.7) declaration:
    ///     declaration-specifiers init-declarator-listopt ;
    ///     static_assert-declaration
    fn declaration(&mut self) -> Result<Spanned<()>> {
        todo!()
    }

    /// (6.7) declaration-specifiers:
    ///   storage-class-specifier declaration-specifiers.opt
    ///   type-specifier declaration-specifiers.opt
    ///   type-qualifier declaration-specifiers.opt
    ///   function-specifier declaration-specifiers.opt
    ///   alignment-specifier declaration-specifiers.opt
    fn declaration_specifiers(&mut self) -> Result<Spanned<DeclSpec>> {
        let mut decl_attr = DeclAttr::default();
        let &(_, initial_span) = self.peek_t()?;
        let (ty, span) = loop {
            match self.peek_t()?.0 {
                // (6.7.1) storage-class-specifier
                Tok::Kw(Kw::Typedef | Kw::Auto | Kw::Register) => {
                    self.next_t()?; // ignore
                }
                Tok::Kw(Kw::Extern) => {
                    self.next_t()?;
                    decl_attr.is_extern = true;
                }
                Tok::Kw(Kw::Static) => {
                    self.next_t()?;
                    decl_attr.is_static = true;
                }
                Tok::Kw(Kw::ThreadLocal) => {
                    self.next_t()?;
                    decl_attr.is_thread_local = true;
                }
                // (6.7.3) type-qualifier:
                Tok::Kw(Kw::Const | Kw::Restrict | Kw::Volatile | Kw::Atomic) => {
                    self.next_t()?; // ignore
                }
                //  (6.7.4) function-specifier:
                Tok::Kw(Kw::Inline | Kw::Noreturn) => {
                    self.next_t()?; // ignore
                }
                // (6.7.5) alignment-specifier:
                Tok::Kw(Kw::Alignas) => {
                    let (token, span) = self.next_t()?;
                    return Err(ParserError::unsupported(span, &token));
                }
                // if it's neither of the above, it has to be a type-specifier
                _ => {
                    let ty = self.type_specifier()?;
                    break ty;
                }
            }
        };

        Ok((
            DeclSpec {
                ty,
                attrs: decl_attr,
            },
            initial_span.extend(span),
        ))
    }

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
            Tok::Kw(Kw::Complex) => {
                return Err(ParserError::new(
                    span,
                    "tf are you doing with complex numbers".to_string(),
                ))
            }
            tok => return Err(ParserError::new(span, format!("Invalid token: `{tok}`"))),
        };
        Ok((ty, span))
    }

    // -----------------------
    // External definitions 
    // -----------------------

    
    /// (6.9) external-declaration:
    ///     function-definition
    ///     declaration
    fn external_declaration(&mut self) -> Result<()> {
        let (next, span) = self.peek_t()?;
        if let Tok::Kw(Kw::StaticAssert) = next {
            return Err(ParserError::unsupported(*span, next));
        }

        let decl_spec = self.declaration_specifiers()?;

        // TODO: We just assume that it's a function, that's terrible!

        self.function_definition(decl_spec)?;

        todo!()
    }

    /// (6.9.1) function-definition:
    ///     declaration-specifiers declarator declaration-list.opt compound-statement
    fn function_definition(&mut self, specifiers: Spanned<DeclSpec>) -> Result<Spanned<()>> {
        todo!()
    }
}
