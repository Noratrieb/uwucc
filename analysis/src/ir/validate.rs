use rustc_hash::FxHashSet;

use super::{Branch, Ir};
use crate::ir::BbIdx;

pub fn validate(ir: &Ir<'_>) {
    for fun in ir.funcs.values() {
        for (i, bb) in fun.bbs.iter().enumerate() {
            if let Branch::Goto(BbIdx(u32::MAX)) = bb.term {
                panic!(
                    "found dummy term in {} in {}",
                    BbIdx::from_usize(i),
                    fun.name
                )
            }
        }

        let mut reg_names = FxHashSet::default();
        for reg in &fun.regs {
            if let Some(name) = reg.name {
                let is_new = reg_names.insert(name);
                if !is_new {
                    panic!("register name {name} is used twice");
                }
            }
        }
    }
}
