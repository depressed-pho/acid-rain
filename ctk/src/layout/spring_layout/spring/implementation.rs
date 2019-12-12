use crate::Component;
use crate::dimension::{
    Dimension,
    LengthRequirements
};
use std::cell::RefCell;
use std::rc::Rc;
use super::{Spring, SpringImpl};

/** A spring whose requirements and values are all fixed. Its method
 * set_value() discards the new value and does nothing.
 */
pub struct StaticSpring {
    length: i32
}

impl StaticSpring {
    pub fn new(length: i32) -> Spring {
        Spring::wrap(Self { length })
    }
}

impl SpringImpl for StaticSpring {
    fn get_requirements(&self) -> LengthRequirements {
        LengthRequirements::exactly(self.length)
    }

    fn get_value(&self) -> i32 {
        self.length
    }

    fn set_value(&mut self, _: i32) {
        // Do nothing.
    }
}

/** A spring whose requirements and value are defined by that of a
 * supplied component. The spring keeps track of changes in the
 * component.
 */
pub struct WidthSpring {
    component: Rc<RefCell<dyn Component>>
}

impl WidthSpring {
    pub fn new(component: Rc<RefCell<dyn Component>>) -> Spring {
        Spring::wrap(Self { component })
    }
}

impl SpringImpl for WidthSpring {
    fn get_requirements(&self) -> LengthRequirements {
        self.component.borrow().get_size_requirements().width
    }

    fn get_value(&self) -> i32 {
        self.component.borrow().get_size().width
    }

    fn set_value(&mut self, width: i32) {
        self.component.borrow_mut()
            .update_size(&|dim| {
                Dimension {
                    width,
                    ..dim
                }});
    }
}

/** A spring whose requirements and value are defined by that of a
 * supplied component. The spring keeps track of changes in the
 * component.
 */
pub struct HeightSpring {
    component: Rc<RefCell<dyn Component>>
}

impl HeightSpring {
    pub fn new(component: Rc<RefCell<dyn Component>>) -> Spring {
        Spring::wrap(Self { component })
    }
}

impl SpringImpl for HeightSpring {
    fn get_requirements(&self) -> LengthRequirements {
        self.component.borrow().get_size_requirements().height
    }

    fn get_value(&self) -> i32 {
        self.component.borrow().get_size().height
    }

    fn set_value(&mut self, height: i32) {
        self.component.borrow_mut()
            .update_size(&|dim| {
                Dimension {
                    height,
                    ..dim
                }});
    }
}
