use peekmore::PeekMoreIterator;

use crate::{
    ast::{
        DeclAttr, DeclSpec, Declaration, Declarator, FunctionDefinition, FunctionParamDecl,
        FunctionParameters, Spanned, TypeSpecifier,
    },
    pre::Punctuator as Punct,
    token::{Keyword as Kw, Token as Tok},
    Span,
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
        Tok::Identifier(_) => false, // TODO: lookup typedefs!
        _ => false,
    }
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

    fn ident(&mut self) -> Result<Spanned<String>> {
        match self.next_t()? {
            (Tok::Identifier(ident), span) => Ok((ident.to_string(), span)),
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

    // -----------------------
    // Declarations
    // -----------------------

    /// (6.7) declaration:
    ///     declaration-specifiers init-declarator-listopt ;
    ///     static_assert-declaration
    fn declaration(&mut self) -> Result<Spanned<Declaration>> {
        if let &(tok @ Tok::Kw(Kw::StaticAssert), span) = self.peek_t()? {
            self.next_t()?;
            return Err(ParserError::unsupported(span, &tok));
        }

        // first, some declaration-specifiers
        let _decl_spec = self.declaration_specifiers()?;
        // then (optionally), a declarator
        // (6.7.6) declarator:
        //      pointer.opt direct-declarator
        if let &(tok @ Tok::Punct(Punct::Asterisk), span) = self.peek_t()? {
            self.next_t()?;
            return Err(ParserError::unsupported(span, &tok));
        }

        // then (optionally), an initializer

        todo!("this doesn't work like this")
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
    fn declarator(&mut self) -> Result<Spanned<Declarator>> {
        todo!()
    }

    // -----------------------
    // External definitions
    // -----------------------

    /// (6.9) external-declaration:
    ///     function-definition
    ///     declaration
    fn external_declaration(&mut self) -> Result<Spanned<FunctionDefinition>> {
        let (next, span) = self.peek_t()?;
        if let Tok::Kw(Kw::StaticAssert) = next {
            return Err(ParserError::unsupported(*span, next));
        }

        let decl_spec = self.declaration_specifiers()?;

        // TODO: We just assume that it's a function, that's terrible!

        self.function_definition(decl_spec)
    }

    /// (6.9.1) function-definition:
    ///     declaration-specifiers declarator declaration-list.opt compound-statement
    /// IMPORTANT TODO DO NOT FORGET THIS IS MISSION CRITICAL
    /// THERE ARE RULES FOR parameter-type-list
    /// WE ARE IN direct-declarator HERE
    /// USE THIS
    /// DO NOT DO THIS WRONGLY
    /// C IS ONLY HALF HAS SHITTY AS IT SOUNDS
    /// OK BUT HALF IS STILL A LOT
    fn function_definition(
        &mut self,
        decl_spec: Spanned<DeclSpec>,
    ) -> Result<Spanned<FunctionDefinition>> {
        let declarator = self.ident()?;

        let decl_spec_span = decl_spec.1;

        expect!(self, Tok::Punct(Punct::ParenOpen));

        let declaration_list = self.function_param_declaration_list()?;

        expect!(self, Tok::Punct(Punct::ParenClose));

        expect!(self, Tok::Punct(Punct::BraceOpen));

        let def = FunctionDefinition {
            decl_spec,
            declarator,
            declaration_list,
            body: Vec::new(),
        };

        let last = expect!(self, Tok::Punct(Punct::BraceClose));

        Ok((def, decl_spec_span.extend(last)))
    }

    fn function_param_declaration_list(&mut self) -> Result<FunctionParameters> {
        // If the declarator includes a parameter type list, the declaration of each parameter shall
        // include an identifier, except for the special case of a parameter list consisting of a single
        // parameter of type void, in which case there shall not be an identifier. No declaration list
        // shall follow.
        if let &(Tok::Kw(Kw::Void), span) = self.peek_t()? {
            if let (Tok::Punct(Punct::ParenClose), _) = self.peek_t_n(1)? {
                self.next_t()?;
                return Ok(FunctionParameters::Void(span));
            }
        }

        let mut params = Vec::new();

        if self.is_peek_tok_start_of_ty() {
            let (decl_spec, span1) = self.declaration_specifiers()?;
            let (declarator, span2) = self.declarator()?;
            let param = FunctionParamDecl {
                decl_spec,
                declarator,
            };

            params.push((param, span1.extend(span2)));
        }

        while self.is_peek_tok_start_of_ty() {
            expect!(self, Tok::Punct(Punct::Comma));
            let (decl_spec, span1) = self.declaration_specifiers()?;
            let (declarator, span2) = self.declarator()?;
            let param = FunctionParamDecl {
                decl_spec,
                declarator,
            };

            params.push((param, span1.extend(span2)));

        }

        Ok(FunctionParameters::List(params))
    }
}

#[cfg(test)]
mod tests;
