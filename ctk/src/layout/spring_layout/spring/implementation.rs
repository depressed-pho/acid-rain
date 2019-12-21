use crate::Component;
use crate::dimension::{
    Dimension,
    LengthRequirements
};
use std::cell::RefCell;
use std::rc::Rc;
use super::{Spring, SpringImpl, SpringSet};

/** A spring whose requirements are fixed at the instantiation time.
 */
#[derive(Clone, Debug)]
pub struct StaticSpring {
    reqs: LengthRequirements,
    length: i32
}

impl StaticSpring {
    pub fn new(reqs: LengthRequirements) -> Spring {
        Spring::wrap(
            Self {
                reqs,
                length: reqs.preferred
            })
    }
}

impl SpringImpl for StaticSpring {
    fn get_requirements(&self) -> LengthRequirements {
        self.reqs
    }

    fn get_length(&self) -> i32 {
        self.length
    }

    fn set_length(&mut self, length: i32) {
        self.length = length;
    }

    fn is_cyclic(&self, seen: SpringSet) -> bool {
        false
    }
}

/** A spring whose requirements and length are defined by that of a
 * supplied component. The spring keeps track of changes in the
 * component.
 */
#[derive(Clone, Debug)]
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

    fn get_length(&self) -> i32 {
        self.component.borrow().get_size().width
    }

    fn set_length(&mut self, width: i32) {
        self.component.borrow_mut()
            .update_size(&|dim| {
                Dimension {
                    width,
                    ..dim
                }});
    }

    fn is_cyclic(&self, seen: SpringSet) -> bool {
        false
    }
}

/** A spring whose requirements and length are defined by that of a
 * supplied component. The spring keeps track of changes in the
 * component.
 */
#[derive(Clone, Debug)]
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

    fn get_length(&self) -> i32 {
        self.component.borrow().get_size().height
    }

    fn set_length(&mut self, height: i32) {
        self.component.borrow_mut()
            .update_size(&|dim| {
                Dimension {
                    height,
                    ..dim
                }});
    }

    fn is_cyclic(&self, seen: SpringSet) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
pub(crate) struct SumSpring {
    a: Spring,
    b: Spring
}

impl SumSpring {
    pub fn new(a: Spring, b: Spring) -> Spring {
        Spring::wrap(Self {a, b})
    }
}

impl SpringImpl for SumSpring {
    fn get_requirements(&self) -> LengthRequirements {
        self.a.get_requirements() + self.b.get_requirements()
    }

    fn get_length(&self) -> i32 {
        self.a.get_length() + self.b.get_length()
    }

    fn set_length(&mut self, length: i32) {
        self.a.set_strain(self.get_strain());
        self.b.set_length(length - self.a.get_length());
    }

    fn is_cyclic(&self, seen: SpringSet) -> bool {
        false
    }
}
