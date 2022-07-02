use dbg_pls::{DebugPls, Formatter};
use peekmore::PeekMoreIterator;

use crate::{
    ast::{
        Decl, DeclAttr, DeclSpec, Declarator, DirectDeclarator, ExternalDecl, FunctionDef,
        FunctionParamDecl, Ident, InitDecl, NormalDecl, TypeSpecifier,
    },
    pre::Punctuator as Punct,
    token::{Keyword as Kw, Token as Tok},
    Span, Spanned,
};

mod expr;

#[derive(Debug)]
pub struct ParserError {
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

impl DebugPls for ParserError {
    fn fmt(&self, f: Formatter<'_>) {
        f.debug_struct("ParserError")
            .field("span", &self.span)
            .field("message", &self.message)
            .finish();
    }
}

type Result<T, E = ParserError> = std::result::Result<T, E>;

struct Parser<'src, I>
where
    I: Iterator<Item = (Tok<'src>, Span)>,
{
    lex: PeekMoreIterator<I>,
}

// HACK: It's called `_parser` as a workaround this being ambiguous with the `#[expect]` attribute
macro_rules! expect_parser {
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

use expect_parser as expect;

macro_rules! eat {
    ($self:ident, $pat:pat) => {
        match $self.peek_t() {
            Ok(($pat, _)) => Some($self.next_t()?),
            _ => None,
        }
    };
}

use eat;

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
    ///     declaration-specifiers init-declarator-list.opt ;
    ///     static_assert-declaration
    fn declaration(&mut self) -> Result<Spanned<Decl>> {
        if let Some((tok, span)) = eat!(self, Tok::Kw(Kw::StaticAssert)) {
            return Err(ParserError::unsupported(span, &tok));
        }

        let (decl_spec, span) = self.decl_specifiers()?;

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
        while self.is_peek_tok_start_of_declarator() || self.is_peek_comma() {
            if !first {
                expect!(self, Tok::Punct(Punct::Comma));
            }
            first = false;

            let (declarator, span) = self.declarator()?;
            let init = if eat!(self, Tok::Punct(Punct::Eq)).is_some() {
                let expr = self.expr()?;
                Some(expr)
            } else {
                None
            };
            let init_decl = InitDecl { declarator, init };
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
    fn decl_specifiers(&mut self) -> Result<Spanned<DeclSpec>> {
        let mut decl_attr = DeclAttr::empty();
        let &(_, initial_span) = self.peek_t()?;
        let (ty, span) = loop {
            match self.peek_t()?.0 {
                // (6.7.1) storage-class-specifier
                Tok::Kw(Kw::Typedef | Kw::Auto | Kw::Register) => {
                    self.next_t()?; // ignore
                }
                Tok::Kw(Kw::Extern) => {
                    self.next_t()?;
                    decl_attr |= DeclAttr::EXTERN;
                }
                Tok::Kw(Kw::Static) => {
                    self.next_t()?;
                    decl_attr |= DeclAttr::STATIC;
                }
                Tok::Kw(Kw::ThreadLocal) => {
                    self.next_t()?;
                    decl_attr |= DeclAttr::THREAD_LOCAL;
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
        let pointer_span = if let Some((_, span)) = eat!(self, Tok::Punct(Punct::Asterisk)) {
            Some(span)
        } else {
            None
        };

        let (decl, span) = self.direct_declarator()?;

        let declarator = Declarator {
            decl,
            pointer: pointer_span.is_some(),
        };

        let span = pointer_span.map(|s| s.extend(span)).unwrap_or(span);

        Ok((declarator, span))
    }

    /// direct-declarator:
    ///     identifier
    ///     ( declarator )
    ///     direct-declarator \[ type-qualifier-list.opt assignment-expression.opt ]
    ///     direct-declarator \[ static type-qualifier-list.opt assignment-expression ]
    ///     direct-declarator \[ type-qualifier-list static assignment-expression ]
    ///     direct-declarator \[ type-qualifier-list.opt * ]
    ///     direct-declarator ( parameter-type-list )
    ///     direct-declarator ( identifier-list.opt )
    fn direct_declarator(&mut self) -> Result<Spanned<DirectDeclarator>> {
        let (ident, span) = self.ident()?;

        if (eat!(self, Tok::Punct(Punct::ParenOpen))).is_some() {
            let mut params = Vec::new();
            let mut first = true;

            while self.is_peek_tok_start_of_ty() || self.is_peek_comma() {
                if first {
                    // the wrong way around because borrowing
                    if let (Tok::Punct(Punct::ParenClose), _) = self.peek_t_n(1)? {
                        if let &(ref tok @ Tok::Kw(Kw::Void), span) = self.peek_t()? {
                            return Err(ParserError::unsupported(span, tok));
                        }
                    }
                }

                if !first {
                    expect!(self, Tok::Punct(Punct::Comma));
                }
                first = false;

                let decl_spec = self.decl_specifiers()?;
                // abstract declarator actually
                let declarator = self.declarator()?;

                let function_param_decl = FunctionParamDecl {
                    decl_spec,
                    declarator,
                };
                params.push(function_param_decl);
            }

            // nothing in the params supported yet.
            expect!(self, Tok::Punct(Punct::ParenClose));
            return Ok((
                DirectDeclarator::WithParams {
                    ident: (ident, span),
                    params,
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
                    decl: declaration,
                    body: Vec::new(),
                }),
                span.extend(span2),
            ))
        } else {
            expect!(self, Tok::Punct(Punct::Semicolon));
            Ok((ExternalDecl::Decl(declaration), span))
        }
    }

    fn external_declarations(&mut self) -> Result<Vec<Spanned<ExternalDecl>>> {
        let mut decls = Vec::new();
        while self.peek_t().is_ok() {
            let decl = self.external_declaration()?;
            decls.push(decl);
        }
        Ok(decls)
    }
}

pub fn parse_declarations<'src>(
    src: impl Iterator<Item = (Tok<'src>, Span)>,
) -> Result<Vec<Spanned<ExternalDecl>>> {
    use peekmore::PeekMore;

    let mut parser = Parser {
        lex: src.peekmore(),
    };

    parser.external_declarations()
}

#[cfg(test)]
mod tests;
