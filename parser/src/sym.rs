use std::{cell::RefCell, fmt::Debug, marker::PhantomData};

use dbg_pls::DebugPls;
use lasso::Spur;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol {
    spur: Spur,
    not_send: PhantomData<*const ()>,
}

thread_local! {
    static INTERNER: RefCell<lasso::Rodeo> = RefCell::new(lasso::Rodeo::new());
}

impl Symbol {
    pub fn intern(s: &str) -> Self {
        INTERNER.with(|i| Symbol {
            spur: i.borrow_mut().get_or_intern(s),
            not_send: PhantomData,
        })
    }

    pub fn as_str<R>(&self, f: impl FnOnce(&str) -> R) -> R {
        INTERNER.with(|i| f(i.borrow_mut().resolve(&self.spur)))
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        INTERNER.with(|i| f.write_str(i.borrow_mut().resolve(&self.spur)))
    }
}

impl DebugPls for Symbol {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        self.as_str(|s| f.debug_ident(s))
    }
}
