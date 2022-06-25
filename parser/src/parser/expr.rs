//! The expression parser is implemented as a pratt parser.
//!
//! For more information, see https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

use crate::{
    ast::{Atom, BinaryOp, Expr, ExprBinary, ExprUnary, UnaryOp},
    parser::{Parser, ParserError, Result},
    pre::Punctuator as P,
    token::{Constant, Token as Tok},
    Span, Spanned,
};

impl<'src, I> Parser<'src, I>
where
    I: Iterator<Item = (Tok<'src>, Span)>,
{
    pub fn expr(&mut self) -> Result<Spanned<Expr>> {
        self.expr_bp(0)
    }

    fn get_lhs(&mut self) -> Result<Spanned<Expr>> {
        let (typ, span) = match self.peek_t()? {
            (Tok::Ident(ident), span) => (Atom::Ident(ident.to_string()), span),
            (Tok::StringLiteral(literal), span) => (Atom::String(literal.to_string()), span),
            (Tok::Constant(Constant::Int(int)), span) => (Atom::Int(*int), span),
            (Tok::Constant(Constant::Float(float)), span) => (Atom::Float(*float), span),
            (Tok::Constant(Constant::Char(char)), span) => (Atom::Char(*char), span),
            &(Tok::Punct(punct), span) => {
                let r_bp = prefix_binding_power(&Tok::Punct(punct));
                let op = unary_op_from_token(&Tok::Punct(punct), span)?;
                let rhs = self.expr_bp(r_bp)?;

                return Ok((
                    Expr::Unary(ExprUnary {
                        rhs: Box::new(rhs),
                        op,
                    }),
                    span,
                ));
            }
            (tok, span) => {
                return Err(ParserError::new(
                    *span,
                    format!("expected expression, found {tok}"),
                ));
            }
        };

        Ok((Expr::Atom(typ), *span))
    }

    fn expr_bp(&mut self, min_bp: u8) -> Result<Spanned<Expr>> {
        let mut lhs = self.get_lhs()?;

        loop {
            let (tok, span) = match self.next_t() {
                Ok(tok) => tok,
                Err(_) => break,
            };
            let op = binary_op_from_token(&tok, span)?;

            let (l_bp, r_bp) = infix_binding_power(&tok);
            if l_bp < min_bp {
                break;
            }

            let rhs = self.expr_bp(r_bp)?;

            let span = lhs.1.extend(rhs.1);

            lhs = (
                Expr::Binary(ExprBinary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op,
                }),
                span,
            )
        }

        todo!()
    }
}

fn unary_op_from_token(tok: &Tok<'_>, span: Span) -> Result<UnaryOp> {
    match tok {
        Tok::Punct(P::Ampersand) => Ok(UnaryOp::AddrOf),
        Tok::Punct(P::Asterisk) => Ok(UnaryOp::Deref),
        Tok::Punct(P::Plus) => Ok(UnaryOp::Plus),
        Tok::Punct(P::Minus) => Ok(UnaryOp::Minus),
        Tok::Punct(P::Tilde) => Ok(UnaryOp::Tilde),
        Tok::Punct(P::Bang) => Ok(UnaryOp::Bang),
        _ => Err(ParserError::new(
            span,
            format!("invalid unary operation: {tok}"),
        )),
    }
}
fn binary_op_from_token(tok: &Tok<'_>, span: Span) -> Result<BinaryOp> {
    match tok {
        Tok::Punct(P::Plus) => Ok(BinaryOp::Add),
        Tok::Punct(P::Minus) => Ok(BinaryOp::Sub),
        _ => Err(ParserError::new(
            span,
            format!("invalid binary operation: {tok}"),
        )),
    }
}

fn prefix_binding_power(tok: &Tok<'_>) -> u8 {
    match tok {
        Tok::Punct(P::Ampersand | P::Asterisk | P::Plus | P::Minus | P::Tilde | P::Bang) => 255,
        _ => panic!("invalid token in expression! {tok:?}"),
    }
}

fn infix_binding_power(tok: &Tok<'_>) -> (u8, u8) {
    match tok {
        Tok::Punct(P::Comma) => (1, 2),
        Tok::Punct(P::Plus | P::Minus) => (3, 4),
        Tok::Punct(P::Asterisk | P::Slash) => (5, 6),
        _ => panic!("invalid token in expression! {tok:?}"),
    }
}
