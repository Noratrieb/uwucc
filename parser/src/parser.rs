use peekmore::PeekMoreIterator;

use crate::{
    ast::{
        Decl, DeclAttr, DeclSpec, Declarator, DirectDeclarator, ExternalDecl, FunctionDef,
        FunctionParamDecl, Ident, InitDecl, IntSign, IntTy, IntTyKind, NormalDecl, Stmt,
        TranslationUnit, TypeSpecifier,
    },
    pre::Punctuator as P,
    sym::Symbol,
    token::{Keyword as Kw, Token as Tok},
    Error, Span, Spanned,
};

mod expr;

impl Error {
    fn eof() -> Self {
        Self::new("unexpected end of file", Span::default())
    }

    fn unsupported(span: Span, token: &Tok<'_>) -> Self {
        Self::new(format!("`{token}` is not supported"), span)
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Parser<'src, I>
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
                return Err(Error::new(
                    format!(
                        concat!("expected `", stringify!($pat), "`, found {}"),
                        token
                    ),
                    span,
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
    matches!(tok, Tok::Ident(_) | Tok::Punct(P::ParenOpen | P::Asterisk))
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
        self.lex.next().ok_or_else(Error::eof)
    }

    fn peek_t(&mut self) -> Result<&(Tok<'src>, Span)> {
        self.lex.peek().ok_or_else(Error::eof)
    }

    fn peek_t_n(&mut self, n: usize) -> Result<&(Tok<'src>, Span)> {
        self.lex.peek_nth(n).ok_or_else(Error::eof)
    }

    fn ident(&mut self) -> Result<Ident> {
        match self.next_t()? {
            (Tok::Ident(ident), span) => Ok((Symbol::intern(ident), span)),
            (tok, span) => Err(Error::new(
                format!("expected identifier, found `{tok}`"),
                span,
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
        matches!(self.peek_t(), Ok((Tok::Punct(P::Comma), _)))
    }

    // -----------------------
    // Declarations
    // -----------------------

    /// (6.7) declaration:
    ///     declaration-specifiers init-declarator-list.opt ;
    ///     static_assert-declaration
    ///
    /// This does NOT eat the semicolon!
    fn declaration(&mut self) -> Result<Spanned<Decl>> {
        if let Some((tok, span)) = eat!(self, Tok::Kw(Kw::StaticAssert)) {
            return Err(Error::unsupported(span, &tok));
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
    ///
    fn init_declarator_list(&mut self) -> Result<Vec<Spanned<InitDecl>>> {
        let mut init_decls = Vec::new();
        let mut first = true;
        while self.is_peek_tok_start_of_declarator() || self.is_peek_comma() {
            if !first {
                expect!(self, Tok::Punct(P::Comma));
            }
            first = false;

            let (declarator, span) = self.declarator()?;
            let init = if eat!(self, Tok::Punct(P::Eq)).is_some() {
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
                    return Err(Error::unsupported(span, &token));
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
        // todo: less shit code and better span handling
        let mut signedness = None;

        loop {
            let (token, span) = self.next_t()?;
            let ty = match token {
                Tok::Kw(Kw::Void) => TypeSpecifier::Void,
                Tok::Kw(Kw::Char) => match signedness {
                    Some(signedness) => TypeSpecifier::Integer(IntTy(signedness, IntTyKind::Char)),
                    None => TypeSpecifier::Char,
                },
                Tok::Kw(Kw::Short) => {
                    eat!(self, Tok::Kw(Kw::Int));
                    TypeSpecifier::Integer(IntTy(signedness.unwrap_or_default(), IntTyKind::Short))
                }
                Tok::Kw(Kw::Int) => {
                    TypeSpecifier::Integer(IntTy(signedness.unwrap_or_default(), IntTyKind::Int))
                }
                Tok::Kw(Kw::Long) => {
                    if eat!(self, Tok::Kw(Kw::Long)).is_some() {
                        eat!(self, Tok::Kw(Kw::Int));
                        TypeSpecifier::Integer(IntTy(
                            signedness.unwrap_or_default(),
                            IntTyKind::LongLong,
                        ))
                    } else {
                        eat!(self, Tok::Kw(Kw::Int));
                        TypeSpecifier::Integer(IntTy(
                            signedness.unwrap_or_default(),
                            IntTyKind::Long,
                        ))
                    }
                }
                Tok::Kw(Kw::Signed) => {
                    if signedness.is_some() {
                        return Err(Error::new("cannot specify signedness twice", span));
                    }
                    if let Ok((Tok::Kw(Kw::Char | Kw::Short | Kw::Int | Kw::Long), _)) =
                        self.peek_t()
                    {
                        // the signed is an integer qualifier
                        signedness = Some(IntSign::Signed);
                        continue;
                    }
                    TypeSpecifier::Integer(IntTy(IntSign::Signed, IntTyKind::Int))
                }
                Tok::Kw(Kw::Unsigned) => {
                    if signedness.is_some() {
                        return Err(Error::new("cannot specify signedness twice", span));
                    }
                    if let Ok((Tok::Kw(Kw::Char | Kw::Short | Kw::Int | Kw::Long), _)) =
                        self.peek_t()
                    {
                        // the unsigned is an integer qualifier
                        signedness = Some(IntSign::Unsigned);
                        continue;
                    }
                    TypeSpecifier::Integer(IntTy(IntSign::Unsigned, IntTyKind::Int))
                }
                Tok::Kw(Kw::Float) => TypeSpecifier::Float,
                Tok::Kw(Kw::Double) => TypeSpecifier::Double,
                Tok::Kw(Kw::Bool) => {
                    TypeSpecifier::Integer(IntTy(IntSign::Unsigned, IntTyKind::Bool))
                }
                Tok::Kw(Kw::Complex) => {
                    return Err(Error::new("tf are you doing with complex numbers", span))
                }
                tok => return Err(Error::new(format!("Invalid token: `{tok}`"), span)),
            };

            break Ok((ty, span));
        }
    }

    /// (6.7.6) declarator:
    ///     pointer.opt direct-declarator
    ///
    /// It's only really known after the parsing of a declarator which kind of declaration we are
    /// facing. For example: `int uwu` vs `int uwu()`. The parentheses indicate a function
    /// declaration. Therefore, we have no idea what we're parsing before entering this function.
    fn declarator(&mut self) -> Result<Spanned<Declarator>> {
        let pointer_span = if let Some((_, span)) = eat!(self, Tok::Punct(P::Asterisk)) {
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

        if (eat!(self, Tok::Punct(P::ParenOpen))).is_some() {
            let mut params = Vec::new();
            let mut first = true;

            while self.is_peek_tok_start_of_ty() || self.is_peek_comma() {
                if first {
                    // the wrong way around because borrowing
                    if let (Tok::Punct(P::ParenClose), _) = self.peek_t_n(1)? {
                        if let &(ref tok @ Tok::Kw(Kw::Void), span) = self.peek_t()? {
                            return Err(Error::unsupported(span, tok));
                        }
                    }
                }

                if !first {
                    expect!(self, Tok::Punct(P::Comma));
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
            expect!(self, Tok::Punct(P::ParenClose));
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
    // Statements
    // -----------------------

    /// (6.8.2) block-item:
    ///     declaration
    ///     statement
    ///
    /// (6.8) statement:
    ///     labeled-statement
    ///     compound-statement
    ///     expression-statement
    ///     selection-statement
    ///     iteration-statement
    ///     jump-statement
    fn statement(&mut self) -> Result<Spanned<Stmt>> {
        if self.is_peek_tok_start_of_ty() {
            let (decl, span) = self.declaration()?;
            let span2 = expect!(self, Tok::Punct(P::Semicolon));
            return Ok((Stmt::Decl(decl), span.extend(span2)));
        }
        // all other stmts are indicated by keywords ...

        if let (Tok::Kw(Kw::If), _) = self.peek_t()? {
            return self.if_statement();
        }

        if let Some((_, span)) = eat!(self, Tok::Kw(Kw::Return)) {
            if let Some((_, semi_span)) = eat!(self, Tok::Punct(P::Semicolon)) {
                return Ok((Stmt::Return(None), span.extend(semi_span)));
            } else {
                let expr = self.expr()?;
                let semi_span = expect!(self, Tok::Punct(P::Semicolon));
                return Ok((Stmt::Return(Some(expr)), span.extend(semi_span)));
            }
        }

        // it must be an expression stmt
        let (expr, span) = self.expr()?;
        expect!(self, Tok::Punct(P::Semicolon));

        Ok((Stmt::Expr(expr), span))
    }

    /// (6.8.2) compound-statement:
    ///     { block-item-listopt }
    ///
    /// The leading `{` must already have been eaten
    fn compound_statement(&mut self, brace_span: Span) -> Result<Spanned<Vec<Spanned<Stmt>>>> {
        let mut stmts = Vec::new();
        let end_span = loop {
            // the end of the block
            if let Some((_, span)) = eat!(self, Tok::Punct(P::BraceClose)) {
                break span;
            }

            // Empty expression statements
            // (6.8.3) expression-statement:
            //      expression.opt ;
            if eat!(self, Tok::Punct(P::Semicolon)).is_some() {
                continue;
            }

            // TODO: recover here
            let stmt = self.statement()?;
            stmts.push(stmt);
        };
        Ok((stmts, brace_span.extend(end_span)))
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
        if let Some((_, brace_span)) = eat!(self, Tok::Punct(P::BraceOpen)) {
            let (body, span2) = self.compound_statement(brace_span)?;

            Ok((
                ExternalDecl::FunctionDef(FunctionDef {
                    decl: declaration,
                    body,
                }),
                span.extend(span2),
            ))
        } else {
            expect!(self, Tok::Punct(P::Semicolon));
            Ok((ExternalDecl::Decl(declaration), span))
        }
    }

    fn external_declarations(&mut self) -> Result<Vec<Spanned<ExternalDecl>>> {
        let mut decls = Vec::new();
        while self.peek_t().is_ok() {
            // TODO recover
            let decl = self.external_declaration()?;
            decls.push(decl);
        }
        Ok(decls)
    }

    fn compount_or_single_statement(&mut self) -> Result<Vec<Spanned<Stmt>>> {
        if let Some((_, brace_span)) = eat!(self, Tok::Punct(P::BraceOpen)) {
            Ok(self.compound_statement(brace_span)?.0)
        } else {
            let stmt = self.statement()?;
            Ok(vec![stmt])
        }
    }

    // TODO: Make sure this is correct.
    fn if_statement(&mut self) -> Result<Spanned<Stmt>> {
        let if_span = expect!(self, Tok::Kw(Kw::If));
        let _paren_span = expect!(self, Tok::Punct(P::ParenOpen));
        let cond = self.expr()?;
        let _paren_span = expect!(self, Tok::Punct(P::ParenClose));
        let then = self.compount_or_single_statement()?;
        let otherwise = if eat!(self, Tok::Kw(Kw::Else)).is_some() {
            Some(self.compount_or_single_statement()?)
        } else {
            None
        };

        let span = if_span
            .extend(cond.1)
            .extend_option(then.last().map(|s| s.1));
        Ok((
            Stmt::If {
                cond,
                then,
                otherwise,
            },
            span,
        ))
    }
}

impl<'src, I> Iterator for Parser<'src, I>
where
    I: Iterator<Item = (Tok<'src>, Span)>,
{
    type Item = Result<Spanned<ExternalDecl>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peek_t().is_ok() {
            let decl = self.external_declaration();
            Some(decl)
        } else {
            None
        }
    }
}

pub fn parse_declarations<'src>(
    src: impl Iterator<Item = (Tok<'src>, Span)>,
) -> Result<TranslationUnit> {
    use peekmore::PeekMore;

    let mut parser = Parser {
        lex: src.peekmore(),
    };

    parser.external_declarations()
}

#[cfg(test)]
mod tests;
