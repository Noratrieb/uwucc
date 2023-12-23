use rustc_hash::FxHashSet;

use super::{visit::Visitor, Branch, Func, Register, StatementKind};
use crate::ir::BbIdx;

pub fn validate(func: &Func<'_>) {
    for (i, bb) in func.bbs.iter().enumerate() {
        if let Branch::Goto(BbIdx(u32::MAX)) = bb.term {
            panic!(
                "found dummy term in {} in {}",
                BbIdx::from_usize(i),
                func.name
            )
        }
    }

    for (i, bb) in func.bbs.iter().enumerate().skip(1) {
        if bb
            .statements
            .iter()
            .any(|stmt| matches!(stmt.kind, StatementKind::Alloca { .. }))
        {
            panic!("alloca is only allowed in first block, found in block {i}")
        }
    }

    let mut reg_names = FxHashSet::default();
    for reg in &func.regs {
        if let Some(name) = reg.name {
            let is_new = reg_names.insert(name);
            if !is_new {
                panic!("register name {name} is used twice");
            }
        }
    }

    ValidationVisitor { func }.visit_func(func);
}

struct ValidationVisitor<'a> {
    func: &'a Func<'a>,
}

impl Visitor for ValidationVisitor<'_> {
    fn visit_reg(&mut self, reg: Register) {
        if self.func.regs.len() <= reg.as_usize() {
            panic!(
                "register out of bounds in {}. %{}, {} registers",
                self.func.name,
                reg.0,
                self.func.regs.len()
            );
        }
    }
}
