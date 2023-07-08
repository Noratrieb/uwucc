use std::{
    cell::Cell,
    fmt::{self, Display, Formatter, Result, Write},
};

use super::{
    BbIdx, BinKind, Branch, ConstValue, Func, Ir, Location, Operand, StatementKind, UnaryKind,
};
use crate::ir::Register;

pub fn ir_to_string<'a>(ir: &'a Ir<'a>, custom: &impl Customizer<'a>) -> String {
    let mut buf = String::new();
    PrettyPrinter { out: &mut buf }.ir(ir, custom).unwrap();
    buf
}

pub fn func_to_string<'a>(func: &'a Func<'a>, custom: &impl Customizer<'a>) -> String {
    let mut buf = String::new();
    PrettyPrinter { out: &mut buf }.func(func, custom).unwrap();
    buf
}

pub trait Customizer<'a> {
    fn start_func(&self, func: &'a Func<'a>);
    fn fmt_reg(&self, reg: Register, f: &mut fmt::Formatter<'_>, loc: Location) -> fmt::Result;
}

#[derive(Default)]
pub struct DefaultCustomizer<'a>(Cell<Option<&'a Func<'a>>>);

impl<'a> Customizer<'a> for DefaultCustomizer<'a> {
    fn start_func(&self, func: &'a Func<'a>) {
        self.0.set(Some(func));
    }

    fn fmt_reg(&self, reg: Register, f: &mut fmt::Formatter<'_>, loc: Location) -> fmt::Result {
        match self.0.get().unwrap().regs[reg.0 as usize].name {
            None => write!(f, "%{}", reg.0),
            Some(name) => write!(f, "%{name}"),
        }
    }
}

pub struct PrettyPrinter<W> {
    out: W,
}

impl<W: Write> PrettyPrinter<W> {
    pub fn ir<'a>(&mut self, ir: &'a Ir<'a>, custom: &impl Customizer<'a>) -> Result {
        for func in ir.funcs.values() {
            self.func(func, custom)?;
        }
        Ok(())
    }

    pub fn func<'a>(&mut self, func: &'a Func<'a>, custom: &impl Customizer<'a>) -> Result {
        custom.start_func(func);

        let print_reg =
            |reg: Register, loc: Location| display_fn(move |f| custom.fmt_reg(reg, f, loc));

        write!(self.out, "def {}(", func.name)?;
        for param in 0..func.arity {
            let reg = &func.regs[param];
            write!(
                self.out,
                "{} {}",
                reg.tyl.ty,
                print_reg(Register(param as _), Location::start())
            )?;
            if (param + 1) != func.arity {
                write!(self.out, ", ")?;
            }
        }
        writeln!(self.out, ") {{",)?;

        let print_op = |op: Operand, loc: Location| {
            display_fn(move |f| match op {
                Operand::Const(c) => Display::fmt(&c, f),
                Operand::Reg(reg) => Display::fmt(&print_reg(reg, loc), f),
            })
        };

        for (i, bb) in func.bbs.iter().enumerate() {
            let bb_idx = BbIdx::from_usize(i);
            if i > 0 {
                writeln!(self.out)?;
            }
            writeln!(self.out, "  {}:", bb_idx)?;

            for (stmt_idx, stmt) in bb.statements.iter().enumerate() {
                let loc = Location::stmt(bb_idx, stmt_idx);
                let print_reg = |reg| print_reg(reg, loc);
                let print_op = |op| print_op(op, loc);

                match stmt.kind {
                    StatementKind::Alloca {
                        result: reg,
                        size,
                        align,
                    } => {
                        writeln!(
                            self.out,
                            "    {} = alloca, size={}, align={}",
                            print_reg(reg),
                            print_op(size),
                            print_op(align)
                        )
                    }
                    StatementKind::Store {
                        ptr: ptr_reg,
                        value,
                        size,
                        align,
                    } => writeln!(
                        self.out,
                        "    store {}, {}, size={}, align={}",
                        print_op(ptr_reg),
                        print_op(value),
                        print_op(size),
                        print_op(align)
                    ),
                    StatementKind::Load {
                        result,
                        ptr: ptr_reg,
                        size,
                        align,
                    } => writeln!(
                        self.out,
                        "    {} = load {}, size={}, align={}",
                        print_reg(result),
                        print_op(ptr_reg),
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
                            UnaryKind::Zext => "zext",
                            UnaryKind::Sext => "sext",
                            UnaryKind::Negate => "negate",
                            UnaryKind::BitNot => "bitnot",
                            UnaryKind::LogicalNot => "logicalnot",
                        },
                        print_op(rhs)
                    ),
                    StatementKind::PtrOffset {
                        result,
                        ptr: reg,
                        amount,
                    } => writeln!(
                        self.out,
                        "    {} = ptroffset {}, {}",
                        print_reg(result),
                        print_op(reg),
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

            let loc = Location::terminator(bb_idx);
            match bb.term {
                Branch::Goto(bbn) => writeln!(self.out, "    goto {}", bbn)?,
                Branch::Switch { cond, yes, no } => writeln!(
                    self.out,
                    "    switch {}, then {yes}, else {no}",
                    print_op(cond, loc)
                )?,
                Branch::Ret(op) => writeln!(self.out, "    ret {}", print_op(op, loc))?,
            }
        }

        writeln!(self.out, "}}")?;

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
