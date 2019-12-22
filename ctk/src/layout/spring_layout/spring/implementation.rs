use crate::Component;
use crate::dimension::{
    Dimension,
    LengthRequirements,
    checked_sub,
    checked_neg
};
use std::cell::RefCell;
use std::fmt::{self, Debug};
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

    fn is_cyclic(&self, _seen: &mut SpringSet) -> bool {
        false
    }
}

/** A spring whose requirements and length are defined by that of a
 * supplied component. The spring keeps track of changes in the
 * component.
 */
#[derive(Clone)]
pub struct WidthSpring {
    of: Rc<RefCell<dyn Component>>
}

impl WidthSpring {
    pub fn new(of: Rc<RefCell<dyn Component>>) -> Spring {
        Spring::wrap(Self { of })
    }
}

impl Debug for WidthSpring {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("WidthSpring")
            .field("of", &self.of.borrow())
            .finish()
    }
}

impl SpringImpl for WidthSpring {
    fn get_requirements(&self) -> LengthRequirements {
        self.of.borrow().get_size_requirements().width
    }

    fn get_length(&self) -> i32 {
        self.of.borrow().get_size().width
    }

    fn set_length(&mut self, width: i32) {
        self.of.borrow_mut()
            .update_size(&|dim| {
                Dimension {
                    width,
                    ..dim
                }});
    }

    fn is_cyclic(&self, _seen: &mut SpringSet) -> bool {
        false
    }
}

/** A spring whose requirements and length are defined by that of a
 * supplied component. The spring keeps track of changes in the
 * component.
 */
#[derive(Clone)]
pub struct HeightSpring {
    of: Rc<RefCell<dyn Component>>
}

impl HeightSpring {
    pub fn new(of: Rc<RefCell<dyn Component>>) -> Spring {
        Spring::wrap(Self { of })
    }
}

impl Debug for HeightSpring {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("HeightSpring")
            .field("of", &self.of.borrow())
            .finish()
    }
}

impl SpringImpl for HeightSpring {
    fn get_requirements(&self) -> LengthRequirements {
        self.of.borrow().get_size_requirements().height
    }

    fn get_length(&self) -> i32 {
        self.of.borrow().get_size().height
    }

    fn set_length(&mut self, height: i32) {
        self.of.borrow_mut()
            .update_size(&|dim| {
                Dimension {
                    height,
                    ..dim
                }});
    }

    fn is_cyclic(&self, _seen: &mut SpringSet) -> bool {
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
        self.b.set_length(checked_sub(length, self.a.get_length()));
    }

    fn is_cyclic(&self, seen: &mut SpringSet) -> bool {
        seen.add(&self.a).add(&self.b).is_cyclic()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct NegativeSpring {
    s: Spring
}

impl NegativeSpring {
    pub fn new(s: Spring) -> Spring {
        Spring::wrap(Self {s})
    }
}

impl SpringImpl for NegativeSpring {
    fn get_requirements(&self) -> LengthRequirements {
        -self.s.get_requirements()
    }

    fn get_length(&self) -> i32 {
        -self.s.get_length()
    }

    fn set_length(&mut self, length: i32) {
        self.s.set_length(checked_neg(length));
    }

    fn is_cyclic(&self, seen: &mut SpringSet) -> bool {
        unimplemented!();
    }
}
