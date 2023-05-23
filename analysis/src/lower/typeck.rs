use parser::{
    ast::{IntSign, IntTy, IntTyKind},
    Span,
};
use smallvec::{smallvec, SmallVec};

use super::{FnLoweringCtxt, Result};
use crate::{
    ty::{Ty, TyKind},
    Error,
};

pub(super) type Coercions<'cx> = SmallVec<[(Coercion, Ty<'cx>); 2]>;

pub(super) enum Coercion {
    ZeroExt,
    SignExt,
    SignToUnsigned,
}

pub(super) struct ArithCoerce<'cx> {
    pub result: Ty<'cx>,
    pub lhs: Coercions<'cx>,
    pub rhs: Coercions<'cx>,
}

impl<'a, 'cx> FnLoweringCtxt<'a, 'cx> {
    /// Performs implicit coercion.
    /// §6.3.1.8 Usual arithmetic conversions
    pub(super) fn arith_op(
        &mut self,
        lhs: Ty<'cx>,
        rhs: Ty<'cx>,
        span: Span,
    ) -> Result<(Ty<'cx>, Coercions<'cx>, Coercions<'cx>)> {
        Ok(match (*lhs, *rhs) {
            (TyKind::LongDouble, _) => (lhs, smallvec![], self.coerce(rhs, lhs)?),
            (_, TyKind::LongDouble) => (rhs, self.coerce(lhs, rhs)?, smallvec![]),
            (TyKind::Double, _) => (lhs, smallvec![], self.coerce(rhs, lhs)?),
            (_, TyKind::Double) => (rhs, self.coerce(lhs, rhs)?, smallvec![]),
            (TyKind::Float, _) => (lhs, smallvec![], self.coerce(rhs, lhs)?),
            (_, TyKind::Float) => (rhs, self.coerce(lhs, rhs)?, smallvec![]),
            (_, _) => {
                let mut lhs_coerce = self.promote(lhs, span)?;
                let mut rhs_coerce = self.promote(rhs, span)?;

                let lhs_prom = lhs_coerce.last().map_or(lhs, |(_, ty)| *ty);
                let rhs_prom = rhs_coerce.last().map_or(rhs, |(_, ty)| *ty);
                let lhs_prom_int = lhs_prom.unwrap_int();
                let rhs_prom_int = rhs_prom.unwrap_int();

                let lhs_sign = lhs_prom_int.sign;
                let rhs_sign = rhs_prom_int.sign;

                let lhs_kind = lhs_prom_int.kind;
                let rhs_kind = rhs_prom_int.kind;

                // If both operands have the same type, then no further conversion is needed.
                let result = if lhs_prom == rhs_prom {
                    lhs
                // Otherwise, if both operands have signed integer types or both have unsigned
                // integer types, the operand with the type of lesser integer conversion rank is
                // converted to the type of the operand with greater rank.
                } else if (lhs_sign.unsigned() && rhs_prom_int.sign.unsigned())
                    || (lhs_sign.signed() && rhs_sign.signed())
                {
                    if lhs_kind > rhs_kind {
                        rhs_coerce.extend(self.coerce(rhs_prom, lhs_prom)?);
                        lhs
                    } else if lhs_kind < rhs_kind {
                        lhs_coerce.extend(self.coerce(lhs_prom, rhs_prom)?);
                        rhs
                    } else {
                        unreachable!("integers must have different rank here")
                    }
                // Otherwise, if the operand that has unsigned integer type has rank greater or
                // equal to the rank of the type of the other operand, then the operand with
                // signed integer type is converted to the type of the operand with unsigned
                // integer type.
                } else if (lhs_sign.unsigned() && lhs_kind >= rhs_kind)
                    || (rhs_sign.unsigned() && rhs_kind >= lhs_kind)
                {
                    if lhs_sign.unsigned() {
                        rhs_coerce.extend(self.coerce(rhs_prom, lhs_prom)?);
                        lhs
                    } else {
                        lhs_coerce.extend(self.coerce(lhs_prom, rhs_prom)?);
                        rhs
                    }
                // Otherwise, if the type of the operand with signed integer type can represent
                // all of the values of the type of the operand with unsigned integer type, then
                // the operand with unsigned integer type is converted to the type of the
                // operand with signed integer type.
                } else if (lhs_sign.unsigned() && is_int_bigger(rhs_kind, lhs_kind))
                    || (rhs_sign.unsigned() && is_int_bigger(lhs_kind, rhs_kind))
                {
                    if lhs_sign.unsigned() {
                        lhs_coerce.extend(self.coerce(lhs_prom, rhs_prom)?);
                        rhs
                    } else {
                        rhs_coerce.extend(self.coerce(rhs_prom, lhs_prom)?);
                        lhs
                    }
                // Otherwise, both operands are converted to the unsigned integer type
                // corresponding to the type of the operand with signed integer type.
                } else {
                    let kind = if lhs_sign.signed() {
                        lhs_kind
                    } else {
                        rhs_kind
                    };
                    let ty = self.lcx.intern_ty(TyKind::Int(IntTy {
                        kind,
                        sign: IntSign::Unsigned,
                    }));
                    lhs_coerce.extend(self.coerce(lhs_prom, ty)?);
                    ty
                };

                (result, lhs_coerce, rhs_coerce)
            }
        })
    }

    fn coerce(&mut self, from: Ty<'cx>, to: Ty<'cx>) -> Result<Coercions<'cx>> {
        if from == to {
            return Ok(smallvec![]);
        }
        Ok(match (*from, *to) {
            (
                TyKind::Char,
                TyKind::Int(IntTy {
                    sign: IntSign::Signed,
                    ..
                }),
            ) => {
                smallvec![(Coercion::SignExt, to)]
            }
            (
                TyKind::Int(IntTy {
                    sign: IntSign::Signed,
                    kind: from_kind,
                }),
                TyKind::Int(IntTy {
                    sign: IntSign::Signed,
                    kind: to_kind,
                }),
            ) if from_kind < to_kind => {
                smallvec![(Coercion::SignExt, to)]
            }
            (
                TyKind::Int(IntTy {
                    sign: IntSign::Unsigned,
                    kind: from_kind,
                }),
                TyKind::Int(IntTy {
                    sign: IntSign::Unsigned,
                    kind: to_kind,
                }),
            ) if from_kind < to_kind => {
                smallvec![(Coercion::ZeroExt, to)]
            }
            _ => panic!("i cant coerce that"),
        })
    }

    // §6.3.1.1 Boolean, characters, and integers
    fn promote(&self, ty: Ty<'cx>, span: Span) -> Result<Coercions<'cx>> {
        Ok(match *ty {
            TyKind::Char => smallvec![(Coercion::SignExt, self.types.int.signed)],
            TyKind::Int(int) if int.kind < IntTyKind::Int => match int.sign {
                IntSign::Signed => smallvec![(Coercion::SignExt, self.types.int.signed)],
                IntSign::Unsigned => {
                    smallvec![(Coercion::ZeroExt, self.types.int.unsigned)]
                }
            },
            TyKind::Int(_) => smallvec![],
            TyKind::Enum(_) => todo!("enums are unimplemented"),
            _ => return Err(Error::new(format!("cannot convert {ty} to integer"), span)),
        })
    }
}

fn is_int_bigger(this: IntTyKind, compared_to: IntTyKind) -> bool {
    let num = |kind| match kind {
        IntTyKind::Bool => 1,
        IntTyKind::Char => 2,
        IntTyKind::Short => 3,
        IntTyKind::Int => 4,
        IntTyKind::Long => 5,
        IntTyKind::LongLong => 5,
    };

    num(this) > num(compared_to)
}
