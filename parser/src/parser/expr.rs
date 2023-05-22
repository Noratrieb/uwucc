//! The expression parser is implemented as a pratt parser.
//!
//! For more information, see https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

use crate::{
    ast::{
        ArithOpKind, Atom, BinaryOp, ComparisonKind, Expr, ExprBinary, ExprPostfix, ExprUnary,
        PostfixOp, UnaryOp,
    },
    parser::{eat, expect, Parser, ParserError, Result},
    pre::Punctuator as P,
    sym::Symbol,
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
            &(Tok::Ident(ident), span) => (Atom::Ident((Symbol::intern(ident), span)), span),
            &(Tok::StringLiteral(literal), span) => (Atom::String(literal.to_string()), span),
            &(Tok::Constant(Constant::Int(int)), span) => (Atom::Int(int), span),
            &(Tok::Constant(Constant::Float(float)), span) => (Atom::Float(float), span),
            &(Tok::Constant(Constant::Char(char)), span) => (Atom::Char(char), span),
            &(Tok::Punct(P::ParenOpen), _) => {
                // TODO: casts... yikes
                self.next_t()?;
                let lhs = self.expr_bp(0);
                expect!(self, Tok::Punct(P::ParenClose));
                return lhs;
            }
            &(Tok::Punct(punct), span) => {
                let r_bp = prefix_binding_power(&Tok::Punct(punct)).ok_or_else(|| {
                    ParserError::new(span, format!("expected expression, found {punct}"))
                })?;
                let Some(op) = unary_op_from_token(&Tok::Punct(punct)) else { panic!() };
                let rhs = self.expr_bp(r_bp)?;

                self.next_t()?;

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

        self.next_t()?;
        Ok((Expr::Atom(typ), span))
    }

    fn expr_bp(&mut self, min_bp: u8) -> Result<Spanned<Expr>> {
        let mut lhs = self.get_lhs()?;

        #[allow(clippy::while_let_loop)] // idc
        loop {
            let (tok, tok_span) = match self.peek_t() {
                Ok(&tok) => tok,
                Err(_) => break,
            };

            if let Some(l_bp) = postfix_binding_power(&tok) {
                if l_bp < min_bp {
                    break;
                }
                let (tok, _) = self.next_t()?;
                match tok {
                    Tok::Punct(P::BracketOpen) => {
                        let rhs = self.expr_bp(0)?;
                        let span = expect!(self, Tok::Punct(P::BracketClose));
                        let span = lhs.1.extend(span);
                        lhs = (
                            Expr::Binary(ExprBinary {
                                lhs: Box::new(lhs),
                                rhs: Box::new(rhs),
                                op: BinaryOp::Index,
                            }),
                            span,
                        );
                    }
                    Tok::Punct(P::ParenOpen) => {
                        let mut arguments = Vec::new();
                        let mut first = true;
                        let last_span;
                        loop {
                            if let Some((_, span)) = eat!(self, Tok::Punct(P::ParenClose)) {
                                last_span = span;
                                break;
                            }
                            if !first {
                                expect!(self, Tok::Punct(P::Comma));
                            }
                            first = false;

                            let arg = self.expr_bp(0)?;
                            arguments.push(arg);
                        }
                        let span = tok_span.extend(last_span);

                        lhs = (
                            Expr::Postfix(ExprPostfix {
                                lhs: Box::new(lhs),
                                op: PostfixOp::Call(arguments),
                            }),
                            span,
                        )
                    }
                    _ => {}
                }
                continue;
            }

            if let Some(op) = binary_op_from_token(&tok) {
                let (l_bp, r_bp) = infix_binding_power(&tok);
                if l_bp < min_bp {
                    break;
                }

                self.next_t()?;

                let rhs = self.expr_bp(r_bp)?;

                let span = lhs.1.extend(rhs.1);

                lhs = (
                    Expr::Binary(ExprBinary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op,
                    }),
                    span,
                );
                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

fn unary_op_from_token(tok: &Tok<'_>) -> Option<UnaryOp> {
    match tok {
        Tok::Punct(P::Ampersand) => Some(UnaryOp::AddrOf),
        Tok::Punct(P::Asterisk) => Some(UnaryOp::Deref),
        Tok::Punct(P::Plus) => Some(UnaryOp::Plus),
        Tok::Punct(P::Minus) => Some(UnaryOp::Minus),
        Tok::Punct(P::Tilde) => Some(UnaryOp::Tilde),
        Tok::Punct(P::Bang) => Some(UnaryOp::Bang),
        _ => None,
    }
}
fn binary_op_from_token(tok: &Tok<'_>) -> Option<BinaryOp> {
    match tok {
        // arithmetic operators
        Tok::Punct(P::Plus) => Some(BinaryOp::Arith(ArithOpKind::Add)),
        Tok::Punct(P::Minus) => Some(BinaryOp::Arith(ArithOpKind::Sub)),
        Tok::Punct(P::Asterisk) => Some(BinaryOp::Arith(ArithOpKind::Mul)),
        Tok::Punct(P::Slash) => Some(BinaryOp::Arith(ArithOpKind::Div)),
        Tok::Punct(P::Percent) => Some(BinaryOp::Arith(ArithOpKind::Mod)),
        Tok::Punct(P::LeftLeftChevron) => Some(BinaryOp::Arith(ArithOpKind::Shl)),
        Tok::Punct(P::RightRightChevron) => Some(BinaryOp::Arith(ArithOpKind::Shr)),
        Tok::Punct(P::Ampersand) => Some(BinaryOp::Arith(ArithOpKind::BitAnd)),
        Tok::Punct(P::Caret) => Some(BinaryOp::Arith(ArithOpKind::BitXor)),
        Tok::Punct(P::Pipe) => Some(BinaryOp::Arith(ArithOpKind::BitOr)),
        // logical operators
        Tok::Punct(P::AmpersandAmpersand) => Some(BinaryOp::LogicalAnd),
        Tok::Punct(P::PipePipe) => Some(BinaryOp::LogicalOr),
        // assignment
        Tok::Punct(P::Eq) => Some(BinaryOp::Assign(None)),
        Tok::Punct(P::PlusEq) => Some(BinaryOp::Assign(Some(ArithOpKind::Add))),
        Tok::Punct(P::MinusEq) => Some(BinaryOp::Assign(Some(ArithOpKind::Sub))),
        Tok::Punct(P::AsteriskEq) => Some(BinaryOp::Assign(Some(ArithOpKind::Mul))),
        Tok::Punct(P::SlashEq) => Some(BinaryOp::Assign(Some(ArithOpKind::Div))),
        Tok::Punct(P::PercentEq) => Some(BinaryOp::Assign(Some(ArithOpKind::Mod))),
        Tok::Punct(P::LeftLeftChevronEq) => Some(BinaryOp::Assign(Some(ArithOpKind::Shl))),
        Tok::Punct(P::RightRightChevronEq) => Some(BinaryOp::Assign(Some(ArithOpKind::Shr))),
        Tok::Punct(P::AmpersandEq) => Some(BinaryOp::Assign(Some(ArithOpKind::BitAnd))),
        Tok::Punct(P::CaretEq) => Some(BinaryOp::Assign(Some(ArithOpKind::BitXor))),
        Tok::Punct(P::PipeEq) => Some(BinaryOp::Assign(Some(ArithOpKind::BitOr))),
        // comparison
        Tok::Punct(P::LeftChevron) => Some(BinaryOp::Comparison(ComparisonKind::Lt)),
        Tok::Punct(P::RightChevron) => Some(BinaryOp::Comparison(ComparisonKind::Gt)),
        Tok::Punct(P::LeftChevronEq) => Some(BinaryOp::Comparison(ComparisonKind::LtEq)),
        Tok::Punct(P::RightChevronEq) => Some(BinaryOp::Comparison(ComparisonKind::GtEq)),
        Tok::Punct(P::EqEq) => Some(BinaryOp::Comparison(ComparisonKind::Eq)),
        Tok::Punct(P::BangEq) => Some(BinaryOp::Comparison(ComparisonKind::Neq)),
        _ => None,
    }
}

mod powers {
    pub const COMMA: (u8, u8) = (1, 2);
    pub const ASSIGN: (u8, u8) = (3, 4);
    pub const TERNARY: (u8, u8) = (5, 6);
    pub const LOGICAL_OR: (u8, u8) = (7, 8);
    pub const LOGICAL_AND: (u8, u8) = (9, 10);
    pub const BIT_OR: (u8, u8) = (11, 12);
    pub const BIT_XOR: (u8, u8) = (13, 14);
    pub const BIT_AND: (u8, u8) = (15, 16);
    pub const EQ_NEQ: (u8, u8) = (17, 18);
    pub const CMP: (u8, u8) = (19, 20);
    pub const SHIFT: (u8, u8) = (21, 22);
    pub const ADD_SUB: (u8, u8) = (23, 24);
    pub const MUL_DIV_MOD: (u8, u8) = (25, 26);
    pub const CAST: u8 = 27;
    pub const UNARY_OPERATOR: u8 = 28;
    pub const POSTFIX: u8 = 29;
}

fn prefix_binding_power(tok: &Tok<'_>) -> Option<u8> {
    Some(match tok {
        Tok::Punct(P::Ampersand | P::Asterisk | P::Plus | P::Minus | P::Tilde | P::Bang) => {
            powers::UNARY_OPERATOR
        }
        _ => return None,
    })
}

fn infix_binding_power(tok: &Tok<'_>) -> (u8, u8) {
    let Tok::Punct(puct) = tok else {
        panic!("invalid token in expression! {tok:?}")
    };

    match puct {
        P::Comma => powers::COMMA,
        P::Eq
        | P::PlusEq
        | P::MinusEq
        | P::AsteriskEq
        | P::SlashEq
        | P::PercentEq
        | P::LeftLeftChevronEq
        | P::RightRightChevronEq
        | P::AmpersandEq
        | P::CaretEq
        | P::PipeEq => powers::ASSIGN,
        // then, ternary here
        P::PipePipe => powers::LOGICAL_OR,
        P::AmpersandAmpersand => powers::LOGICAL_AND,
        P::Pipe => powers::BIT_OR,
        P::Caret => powers::BIT_XOR,
        P::Ampersand => powers::BIT_AND,
        P::EqEq | P::BangEq => powers::EQ_NEQ,
        P::LeftChevron | P::RightChevron | P::LeftChevronEq | P::RightChevronEq => powers::CMP,
        P::LeftLeftChevron | P::RightRightChevron => powers::SHIFT,
        P::Plus | P::Minus => powers::ADD_SUB,
        P::Asterisk | P::Slash | P::Percent => powers::MUL_DIV_MOD,
        _ => panic!("invalid token in expression! {tok:?}"),
    }
}

fn postfix_binding_power(tok: &Tok<'_>) -> Option<u8> {
    match tok {
        Tok::Punct(P::BracketOpen) => Some(powers::POSTFIX),
        Tok::Punct(P::ParenOpen) => Some(powers::POSTFIX),
        Tok::Punct(P::Dot) => Some(powers::POSTFIX),
        Tok::Punct(P::Arrow) => Some(powers::POSTFIX),
        Tok::Punct(P::PlusPlus) => Some(powers::POSTFIX),
        Tok::Punct(P::MinusMinus) => Some(powers::POSTFIX),
        _ => None,
    }
}
