//! # Register allocation
//!
//! Register allocation is not very smart, but also not too stupid. It tries to put SSA
//! registers into machine registers as much as possible.
//!
//! ```text
//! bb0:
//!   %0 = 0
//!   %1 = 1
//!   %2 = add %0 %1
//!   switch %2, then bb1, else bb2
//!
//! bb1:
//!   %3 = add %1, 1
//!   
//! bb2:
//!   %4 = add %2, 2
//! ```
//!
//! For all SSA registers, we establish their "point of last use". This is the bb,stmt where their last usage occurs.
//!
//! First, we establish a list of possible registers to allocate.
//! Since we immediately alloca all parameters, all the param registers are free real estate.
//!
//! ```text
//! rax, rdi, rsi, rcx, rdx, r8, r9
//! ```
//!
//! This forms our priority list of registers.
//!
//! Every time a statement has a return value, we try to assign that SSA register into a new machine register.
//! For this, we iterate through the register list above and find the first register that's free. If we see a register
//! that is not used anymore at the current location, we throw it out and use that new slot.
//!
//! When codegening an SSA register, we look into a lookup table from SSA register to machine register/stack spill and use that.
//!
//! When the list above is full, we spill the register to the stack. This should be rare. If the register doesn't fit into a machine
//! register, it's also spilled.
//!
//! We do a first pass over the function to calculate all the offsets and registers
//! we want to use.

use std::cell::Cell;

use analysis::ir::{self, Func, Location, Register};
use rustc_hash::FxHashMap;

/// A machine register from our register list described in the module documentation.
#[derive(Debug, Clone, Copy)]
pub struct MachineReg(pub usize);

#[derive(Debug, Clone, Copy)]
pub enum RegValue {
    /// The SSA register contains an address on the stack.
    /// The offset is the offset from the start of the function.
    StackRelativePtr { offset: u64 },
    /// The SSA register resides on the stack as it has been spilled.
    /// This should be rather rare in practice.
    Spilled { offset: u64 },
    /// The SSA register resides in a machine register.
    MachineReg(MachineReg),
}

#[derive(Debug)]
pub struct FunctionLayout {
    /// Where a register comes from at a particular usage of a register.
    register_uses: FxHashMap<(Location, Register), RegValue>,
}

pub fn compute_layout(_f: &Func) -> FunctionLayout {
    let register_uses = FxHashMap::default();

    FunctionLayout {
        register_uses,
    }
}

pub struct LayoutPrinter<'a>(Cell<Option<&'a Func<'a>>>, &'a FunctionLayout);

impl<'a> ir::pretty::Customizer<'a> for LayoutPrinter<'a> {
    fn start_func(&self, func: &'a Func<'a>) {
        self.0.set(Some(func));
    }

    fn fmt_reg(
        &self,
        reg: Register,
        f: &mut std::fmt::Formatter<'_>,
        loc: Location,
    ) -> std::fmt::Result {
        let layout = self.1.register_uses.get(&(loc, reg));
        write!(f, "{{")?;

        match self.0.get().unwrap().regs[reg.0 as usize].name {
            None => write!(f, "%{}", reg.0)?,
            Some(name) => write!(f, "%{name}")?,
        }

        write!(f, ", ")?;
        match layout {
            Some(RegValue::MachineReg(mach)) => write!(f, "reg-{}", mach.0)?,
            Some(RegValue::Spilled { offset }) => write!(f, "spill-{offset}")?,
            Some(RegValue::StackRelativePtr { offset }) => {
                write!(f, "i-forgot-what-this-meant-{offset}")?
            }
            None => write!(f, "<unknown>")?,
        }

        write!(f, "}}")
    }
}

pub fn debug_layout(func: &Func, layout: &FunctionLayout) {
    let custom = LayoutPrinter(Cell::default(), layout);

    println!("----- code layout");
    println!("{}", ir::pretty::func_to_string(func, &custom));

    dbg!(layout);
}
