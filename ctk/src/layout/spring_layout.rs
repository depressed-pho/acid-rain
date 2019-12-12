mod constraints;
pub use constraints::*;

mod spring;
pub use spring::*;

use crate::{
    Component,
    Layout
};
use crate::dimension::SizeRequirements;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use weak_table::PtrWeakKeyHashMap;

pub struct SpringLayout {
    /* Invariant: Springs are all acyclic, including proxies and
     * implicit ones. This invariant is VERY hard to validate.
     */
    components: Vec<Rc<RefCell<dyn Component>>>,
    constraints: PtrWeakKeyHashMap<Weak<RefCell<dyn Component>>, Rc<RefCell<Constraints>>>,
    parent_constr: Rc<RefCell<Constraints>>,
    is_valid: bool
}

impl SpringLayout {
    pub fn new() -> Self {
        let mut con = Constraints::new();
        con.set_spring(Edge::Left, Some(StaticSpring::new(0)));
        con.set_spring(Edge::Top , Some(StaticSpring::new(0)));
        Self {
            components: Vec::new(),
            constraints: PtrWeakKeyHashMap::new(),
            parent_constr: Rc::new(RefCell::new(con)),
            is_valid: true
        }
    }

    pub fn add(&mut self, c: Rc<RefCell<dyn Component>>) -> &mut Self {
        self.components.push(c.clone());

        /* The rules for default springs are as follows:
         *
         * - For edges that have no sensible defaults and always need
         *   explicit attention from users, they have no defaults even
         *   that would lead to panics at runtime if unattended. The
         *   Left and Top edges are such examples.
         *
         * - For edges that have a sensible default which makes sense
         *   to most users, they have a default spring. The width and
         *   height "edges" are examples.
         *
         * - We don't inject any default springs into Constraint
         *   objects after exposing them to users. That is, if a user
         *   removes the width spring and only leaves a left spring,
         *   the layout manager just panics as opposed to reinstalling
         *   a width spring.
         */
        let mut con = Constraints::new();
        con.set_spring(Edge::Width, Some(WidthSpring::new(c.clone())));
        con.set_spring(Edge::Height, Some(HeightSpring::new(c.clone())));

        self.constraints.insert(c, Rc::new(RefCell::new(con)));
        self.invalidate();
        self
    }

    pub fn remove(&mut self, c: Rc<RefCell<dyn Component>>) -> &mut Self {
        self.components.retain(|c_| !Rc::ptr_eq(&c, &c_));
        self.constraints.remove(&c);
        self.invalidate();
        self
    }

    /** Return the Constraints object associated with a given
     * Component, or None if no such Component has been added to this
     * SpringLayout.
     *
     * This method is unsafe because callers can accidentally create
     * cycles in springs.
     */
    unsafe fn get_constraints(&self, c: Rc<RefCell<dyn Component>>) -> Option<Rc<RefCell<Constraints>>> {
        self.constraints.get(&c).cloned()
    }
}

impl Layout for SpringLayout {
    fn validate(&mut self, parent: &dyn Component) {
        unimplemented!();
    }

    fn invalidate(&mut self) {
        self.is_valid = false
    }

    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Rc<RefCell<dyn Component>>> + 'a> {
        Box::new(self.components.iter())
    }

    fn get_size_requirements(&self, parent: &dyn Component) -> SizeRequirements {
        unimplemented!();
    }
}
