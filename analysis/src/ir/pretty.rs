use std::fmt::{Display, Formatter, Result, Write};

use super::{BbIdx, BinKind, Branch, ConstValue, Func, Ir, Operand, StatementKind, UnaryKind};
use crate::ir::Register;

pub fn ir_to_string(ir: &Ir<'_>) -> String {
    let mut buf = String::new();
    PrettyPrinter { out: &mut buf }.ir(ir).unwrap();
    buf
}

pub fn func_to_string(func: &Func<'_>) -> String {
    let mut buf = String::new();
    PrettyPrinter { out: &mut buf }.func(func).unwrap();
    buf
}

pub struct PrettyPrinter<W> {
    out: W,
}

impl<W: Write> PrettyPrinter<W> {
    pub fn ir(&mut self, ir: &Ir<'_>) -> Result {
        for func in ir.funcs.values() {
            self.func(func)?;
        }
        Ok(())
    }

    pub fn func(&mut self, func: &Func<'_>) -> Result {
        let print_reg = |reg: Register| {
            display_fn(move |f| match func.regs[reg.0 as usize].name {
                None => write!(f, "%{}", reg.0),
                Some(name) => write!(f, "%{name}"),
            })
        };

        write!(self.out, "def {}(", func.name)?;
        for param in 0..func.arity {
            let reg = &func.regs[param as usize];
            write!(self.out, "{} {}", reg.tyl.ty, print_reg(Register(param)))?;
            if (param + 1) != func.arity {
                write!(self.out, ", ")?;
            }
        }
        writeln!(self.out, ") {{",)?;

        let print_op = |op: Operand| {
            display_fn(move |f| match op {
                Operand::Const(c) => Display::fmt(&c, f),
                Operand::Reg(reg) => Display::fmt(&print_reg(reg), f),
            })
        };

        for (i, bb) in func.bbs.iter().enumerate() {
            if i > 0 {
                writeln!(self.out)?;
            }
            writeln!(self.out, "  {}:", BbIdx::from_usize(i))?;

            for stmt in &bb.statements {
                match stmt.kind {
                    StatementKind::Alloca { reg, size, align } => {
                        writeln!(
                            self.out,
                            "    {} = alloca, size={}, align={}",
                            print_reg(reg),
                            print_op(size),
                            print_op(align)
                        )
                    }
                    StatementKind::Store {
                        ptr_reg,
                        value,
                        size,
                        align,
                    } => writeln!(
                        self.out,
                        "    store {}, {}, size={}, align={}",
                        print_reg(ptr_reg),
                        print_op(value),
                        print_op(size),
                        print_op(align)
                    ),
                    StatementKind::Load {
                        result,
                        ptr_reg,
                        size,
                        align,
                    } => writeln!(
                        self.out,
                        "    {} = load {}, size={}, align={}",
                        print_reg(result),
                        print_reg(ptr_reg),
                        print_op(size),
                        print_op(align)
                    ),
                    StatementKind::BinOp {
                        kind,
                        lhs,
                        rhs,
                        result,
                    } => writeln!(
                        self.out,
                        "    {} = {} {}, {}",
                        print_reg(result),
                        match kind {
                            BinKind::Add => "add",
                            BinKind::Sub => "sub",
                            BinKind::Mul => "mul",
                            BinKind::Div => "div",
                            BinKind::Mod => "mod",
                            BinKind::Eq => "eq",
                            BinKind::Neq => "neq",
                            BinKind::Gt => "gt",
                            BinKind::Geq => "geq",
                            BinKind::Lt => "gl",
                            BinKind::Leq => "leq",
                            BinKind::Shl => "shl",
                            BinKind::Shr => "shr",
                            BinKind::BitAnd => "bitand",
                            BinKind::BitOr => "bitor",
                            BinKind::BitXor => "bitxor",
                        },
                        print_op(lhs),
                        print_op(rhs)
                    ),
                    StatementKind::UnaryOperation { rhs, kind, result } => writeln!(
                        self.out,
                        "    {} = {} {}",
                        print_reg(result),
                        match kind {
                            UnaryKind::Negate => "negate",
                            UnaryKind::BitNot => "bitnot",
                            UnaryKind::LogicalNot => "logicalnot",
                        },
                        print_op(rhs)
                    ),
                    StatementKind::PtrOffset {
                        result,
                        reg,
                        amount,
                    } => writeln!(
                        self.out,
                        "    {} = ptroffset {}, {}",
                        print_reg(result),
                        print_reg(reg),
                        print_op(amount)
                    ),
                    StatementKind::Call {
                        result,
                        func,
                        ref args,
                    } => {
                        writeln!(
                            self.out,
                            "    {} = call {} ({})",
                            print_reg(result),
                            print_op(func),
                            args.iter()
                                .map(|arg| print_op(*arg).to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                }?;
            }

            match bb.term {
                Branch::Goto(bbn) => writeln!(self.out, "    goto {}", bbn)?,
                Branch::Switch { cond, yes, no } => writeln!(
                    self.out,
                    "    switch {}, then {yes}, else {no}",
                    print_op(cond)
                )?,
                Branch::Ret(op) => writeln!(self.out, "    ret {}", print_op(op))?,
            }
        }

        Ok(())
    }
}

impl Display for ConstValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            ConstValue::Int(int) => <_ as Display>::fmt(int, f),
            ConstValue::Void => f.write_str("void"),
            ConstValue::StaticPtr(def_id) => write!(f, "{{{}}}", def_id.0),
        }
    }
}

fn display_fn<F: Fn(&mut Formatter<'_>) -> Result>(f: F) -> impl Display {
    struct DisplayFn<F> {
        f: F,
    }

    impl<F: Fn(&mut Formatter<'_>) -> Result> Display for DisplayFn<F> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            (self.f)(f)
        }
    }

    DisplayFn { f }
}
