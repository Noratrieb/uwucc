pub mod help {
    use crate::ir::{ConstValue, Operand, Register};

    pub trait ToOperand {
        fn to_operand(self) -> Operand;
    }

    pub fn op(o: impl ToOperand) -> Operand {
        o.to_operand()
    }

    impl ToOperand for Register {
        fn to_operand(self) -> Operand {
            Operand::Reg(self)
        }
    }

    impl ToOperand for u64 {
        fn to_operand(self) -> Operand {
            Operand::Const(ConstValue::u64(self))
        }
    }
}

#[macro_export]
macro_rules! define_ir_func {
    (@bbs($bb:ident) { $( $stmt:expr );* }) => {
        $(
            let s = $crate::ir::Statement {
                span: ::parser::Span::dummy(),
                kind: {
                    #[allow(unused_imports)]
                    use $crate::ir::{StatementKind::*, Register, custom_help::*, BinKind};
                    $stmt
                },
            };
            $bb.statements.push(s);
        )*
    };
    (@body($f:ident) { $($num:literal : { $( $stmt:expr );* $(;)? => $branch:expr })* }) => {
        $(
            assert_eq!($f.bbs.len(), $num);
            #[allow(unused_mut)]
            let mut bb = $crate::ir::BasicBlock {
                statements: Vec::new(),
                term: {
                    #[allow(unused_imports)]
                    use $crate::ir::{Register, custom_help::*, Branch::*};
                    $branch
                },
            };
            $crate::ir::define_ir_func! { @bbs(bb) { $($stmt);* } };
            $f.bbs.push(bb);
        )*
    };

    // entrypoint
    (
        def($lcx:ident) $name:ident (), regs($regs:literal) {
            $($body:tt)*
        }
    ) => {{
        let mut f = $crate::ir::Func {
            name: ::parser::Symbol::intern(stringify!($name)),
            def_span: ::parser::Span::dummy(),
            arity: 0,
            ret_ty: $lcx.intern_ty($crate::ty::TyKind::Void),
            bbs: Vec::new(),
            regs: vec![$crate::ir::RegisterData {
                tyl: $lcx.layout_of($lcx.types.int.unsigned),
                name: None,
            }; $regs],
        };
        $crate::ir::define_ir_func! { @body(f) { $($body)* } };
        $crate::ir::validate(&f);
        f
    }};
}

pub use define_ir_func;

#[cfg(test)]
mod tests {
    use crate::LoweringCx;

    #[test]
    fn define() {
        let arena = bumpalo::Bump::new();
        let lcx = LoweringCx::new(&arena);

        let _f = define_ir_func! {
            def(lcx) name (), regs(1) {
                0: {
                    BinOp { result: Register(0), kind: BinKind::Add, lhs: op(0), rhs: op(2) };
                    => Ret(op(Register(0)))
                }
            }
        };
    }
}
