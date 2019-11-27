mod constraints;
pub use constraints::*;

mod spring;
pub use spring::*;

use crate::ctk::{
    Component,
    Layout
};
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use weak_table::PtrWeakKeyHashMap;

pub struct SpringLayout {
    components: Vec<Rc<RefCell<dyn Component>>>,
    constraints: PtrWeakKeyHashMap<Weak<RefCell<dyn Component>>, Constraints>,
    parent_constr: Constraints,
    is_valid: bool
}

impl SpringLayout {
    pub fn new() -> Self {
        Self {
            components: Vec::new(),
            constraints: PtrWeakKeyHashMap::new(),
            parent_constr: Constraints::new(),
            is_valid: true
        }
    }
}
