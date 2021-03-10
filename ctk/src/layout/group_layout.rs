use crate::{
    Component,
    ComponentRef,
    Layout
};
use crate::dimension::{
    SizeRequirements
};

/// This layout manager is not finished, and will probably never be.
#[derive(Debug)]
pub struct GroupLayout {
    auto_create_gaps: bool,
    auto_create_container_gaps: bool,
    is_valid: bool
}

impl GroupLayout {
    pub fn new() -> Self {
        Self {
            auto_create_gaps: false,
            auto_create_container_gaps: false,
            is_valid: false
        }
    }

    pub fn set_auto_create_gaps(&mut self, b: bool) -> &mut Self {
        self.auto_create_gaps = b;
        self.invalidate();
        self
    }

    pub fn get_auto_create_gaps(&self) -> bool {
        self.auto_create_gaps
    }

    pub fn set_auto_create_container_gaps(&mut self, b: bool) -> &mut Self {
        self.auto_create_container_gaps = b;
        self.invalidate();
        self
    }

    fn do_layout(&mut self, _parent: &dyn Component) {
        unimplemented!();
    }
}

impl Layout for GroupLayout {
    fn validate(&mut self, this: &dyn Component, _this_ref: &ComponentRef<dyn Component>) {
        if !self.is_valid {
            self.do_layout(this);
            self.is_valid = true;
        }
        /* FIXME
        for child in self.components.iter() {
            let mut child = child_.borrow_mut();
            child.set_parent(Some(this_ref));
            child.validate(child_);
        }
         */
        unimplemented!();
    }

    fn invalidate(&mut self) {
        self.is_valid = false
    }

    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a ComponentRef<dyn Component>> + 'a> {
        unimplemented!();
    }

    fn get_size_requirements(&self, _parent: &dyn Component) -> SizeRequirements {
        unimplemented!();
    }
}
