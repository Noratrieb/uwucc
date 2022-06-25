use peekmore::PeekMoreIterator;

use crate::{
    ast::{
        Decl, DeclAttr, DeclSpec, Declarator, DirectDeclarator, ExternalDecl, FunctionDef,
        FunctionParams, Ident, InitDecl, NormalDecl, TypeSpecifier,
    },
    pre::Punctuator as Punct,
    token::{Keyword as Kw, Token as Tok},
    Span, Spanned,
};

#[derive(Debug)]
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

macro_rules! expect {
    ($self:ident, $pat:pat) => {
        match $self.next_t()? {
            ($pat, span) => span,
            (token, span) => {
                return Err(ParserError::new(
                    span,
                    format!(
                        concat!("expected `", stringify!($pat), "`, found {}"),
                        token
                    ),
                ))
            }
        }
    };
}

macro_rules! eat {
    ($self:ident, $pat:pat) => {
        match $self.peek_t() {
            Ok(($pat, _)) => Some($self.next_t()?),
            _ => None,
        }
    };
}

/// Can be called for the start of a sequence of tokens that could be a type.
#[rustfmt::skip]
fn is_tok_start_of_ty(tok: &Tok<'_>) -> bool {
    match tok {
        Tok::Kw(
            // storage specifiers
            Kw::Typedef | Kw::Extern | Kw::Static | Kw::ThreadLocal | Kw::Auto | Kw::Register
            // primitive types
            | Kw::Void | Kw::Char | Kw::Short | Kw::Int | Kw::Long | Kw::Float | Kw::Double
            | Kw::Signed | Kw::Unsigned | Kw::Bool | Kw::Complex
            // struct-union-enum
            | Kw::Struct | Kw::Union | Kw::Enum
            // atomic
            | Kw::Atomic
            // type qualifiers
            | Kw::Const | Kw::Restrict | Kw::Volatile /* atomic too */
            // function specifiers
            | Kw::Inline | Kw::Noreturn
        ) => true,
        Tok::Ident(_) => false, // TODO: lookup typedefs!
        _ => false,
    }
}

/// Can be called for the start of a sequence of tokens that could be a type.
fn is_tok_start_of_declarator(tok: &Tok<'_>) -> bool {
    matches!(
        tok,
        Tok::Ident(_) | Tok::Punct(Punct::ParenOpen | Punct::Asterisk)
    )
}

impl<'src, I> Parser<'src, I>
where
    I: Iterator<Item = (Tok<'src>, Span)>,
{
    // -----------------------
    // Helpers
    // -----------------------

    fn find_typedef(&self, _: &str) -> Option<()> {
        None // TODO: this
    }

    fn next_t(&mut self) -> Result<(Tok<'src>, Span)> {
        self.lex.next().ok_or_else(ParserError::eof)
    }

    fn peek_t(&mut self) -> Result<&(Tok<'src>, Span)> {
        self.lex.peek().ok_or_else(ParserError::eof)
    }

    fn peek_t_n(&mut self, n: usize) -> Result<&(Tok<'src>, Span)> {
        self.lex.peek_nth(n).ok_or_else(ParserError::eof)
    }

    fn ident(&mut self) -> Result<Ident> {
        match self.next_t()? {
            (Tok::Ident(ident), span) => Ok((ident.to_string(), span)),
            (tok, span) => Err(ParserError::new(
                span,
                format!("expected identifier, found `{tok}`"),
            )),
        }
    }

    fn is_peek_tok_start_of_ty(&mut self) -> bool {
        match self.peek_t() {
            Ok((tok, _)) => is_tok_start_of_ty(tok),
            Err(_) => false,
        }
    }

    fn is_peek_tok_start_of_declarator(&mut self) -> bool {
        match self.peek_t() {
            Ok((tok, _)) => is_tok_start_of_declarator(tok),
            Err(_) => false,
        }
    }

    fn is_peek_comma(&mut self) -> bool {
        matches!(self.peek_t(), Ok((Tok::Punct(Punct::Comma), _)))
    }

    // -----------------------
    // Declarations
    // -----------------------

    /// (6.7) declaration:
    ///     declaration-specifiers init-declarator-listopt ;
    ///     static_assert-declaration
    fn declaration(&mut self) -> Result<Spanned<Decl>> {
        if let Some((tok, span)) = eat!(self, Tok::Kw(Kw::StaticAssert)) {
            return Err(ParserError::unsupported(span, &tok));
        }

        let (decl_spec, span) = self.declaration_specifiers()?;

        let init_declarators = self.init_declarator_list()?;
        let init_declarators_span = Span::span_of_spanned_list(&init_declarators);

        let span = span.extend_option(init_declarators_span);

        Ok((
            Decl::Normal(NormalDecl {
                decl_spec,
                init_declarators,
            }),
            span,
        ))
    }

    /// init-declarator-list:
    ///     init-declarator
    ///     init-declarator-list , init-declarator
    ///
    /// init-declarator:
    ///     declarator
    ///     declarator = initializer
    fn init_declarator_list(&mut self) -> Result<Vec<Spanned<InitDecl>>> {
        let mut init_decls = Vec::new();
        let mut first = true;
        loop {
            println!("LOOOOP");
            if !self.is_peek_tok_start_of_declarator() && !self.is_peek_comma() {
                break;
            }
            if !first {
                expect!(self, Tok::Punct(Punct::Comma));
            }
            first = false;

            let (declarator, span) = self.declarator()?;
            if let Some((token, span)) = eat!(self, Tok::Punct(Punct::Eq)) {
                return Err(ParserError::unsupported(span, &token));
            }
            let init_decl = InitDecl {
                declarator,
                init: None,
            };
            init_decls.push((init_decl, span));
        }

        Ok(init_decls)
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

    /// (6.7.6) declarator:
    ///     pointer.opt direct-declarator
    ///
    /// It's only really known after the parsing of a declarator which kind of declaration we are
    /// facing. For example: `int uwu` vs `int uwu()`. The parentheses indicate a function
    /// declaration. Therefore, we have no idea what we're parsing before entering this function.
    fn declarator(&mut self) -> Result<Spanned<Declarator>> {
        if let Some((_, span)) = eat!(self, Tok::Punct(Punct::Asterisk)) {
            let (decl, span2) = self.direct_declarator()?;

            Ok((
                Declarator {
                    decl,
                    pointer: true,
                },
                span.extend(span2),
            ))
        } else {
            let (decl, span) = self.direct_declarator()?;

            Ok((
                Declarator {
                    decl,
                    pointer: false,
                },
                span,
            ))
        }
    }

    /// direct-declarator:
    ///     identifier
    ///     ( declarator )
    ///     direct-declarator \[ type-qualifier-listopt assignment-expressionopt ]
    ///     direct-declarator \[ static type-qualifier-listopt assignment-expression ]
    ///     direct-declarator \[ type-qualifier-list static assignment-expression ]
    ///     direct-declarator \[ type-qualifier-listopt * ]
    ///     direct-declarator ( parameter-type-list )
    ///     direct-declarator ( identifier-listopt )
    fn direct_declarator(&mut self) -> Result<Spanned<DirectDeclarator>> {
        let (ident, span) = self.ident()?;

        if (eat!(self, Tok::Punct(Punct::ParenOpen))).is_some() {
            // nothing in the params supported yet.
            expect!(self, Tok::Punct(Punct::ParenClose));
            return Ok((
                DirectDeclarator::WithParams {
                    ident: (ident, span),
                    params: Vec::new(),
                },
                span,
            ));
        }

        Ok((DirectDeclarator::Ident((ident, span)), span))
    }

    // -----------------------
    // External definitions
    // -----------------------

    /// (6.9) external-declaration:
    ///     function-definition
    ///     declaration
    fn external_declaration(&mut self) -> Result<Spanned<ExternalDecl>> {
        let (declaration, span) = self.declaration()?;

        // the declaration might be a function definition
        if eat!(self, Tok::Punct(Punct::BraceOpen)).is_some() {
            let span2 = expect!(self, Tok::Punct(Punct::BraceClose));

            Ok((
                ExternalDecl::FunctionDef(FunctionDef {
                    declaration,
                    body: Vec::new(),
                }),
                span.extend(span2),
            ))
        } else {
            Ok((ExternalDecl::Decl(declaration), span))
        }
    }

    fn function_param_declaration_list(&mut self) -> Result<FunctionParams> {
        // If the declarator includes a parameter type list, the declaration of each parameter shall
        // include an identifier, except for the special case of a parameter list consisting of a single
        // parameter of type void, in which case there shall not be an identifier. No declaration list
        // shall follow.
        if let &(Tok::Kw(Kw::Void), span) = self.peek_t()? {
            if let (Tok::Punct(Punct::ParenClose), _) = self.peek_t_n(1)? {
                self.next_t()?;
                self.next_t()?;
                return Ok(FunctionParams::Void(span));
            }
        }

        todo!()
    }
}

#[cfg(test)]
mod tests;