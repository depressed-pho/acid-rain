use crate::Component;
use crate::dimension::{
    LengthRequirements
};
use lazy_static::lazy_static;
use std::cell::RefCell;
use std::fmt::{self, Debug};
use std::rc::Rc;
use super::{Spring, SpringImpl, SpringSet};

lazy_static! {
    /* In order to evenly distribute extra spaces, it is crucial to
     * clamp the maximum length of non-compositing springs to
     * somewhere under the i32::max_value(). Otherwise the important
     * invariant s1.range() + s2.range() = (s1 + s2).range() will not
     * hold.
     */
    static ref MAX: LengthRequirements =
        LengthRequirements::at_most(i16::max_value() as i32);
}

/// A spring whose requirements are fixed at the instantiation time.
#[derive(Clone, Debug)]
pub struct StaticSpring {
    reqs: LengthRequirements,
    length: i32
}

impl StaticSpring {
    pub fn new(reqs: LengthRequirements) -> Spring {
        Spring::wrap(
            Self {
                reqs: reqs & *MAX,
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

/// A spring whose requirements and length are defined by that of a
/// supplied component. The spring keeps track of changes in the
/// component.
#[derive(Clone)]
pub struct WidthSpring {
    of: Rc<RefCell<dyn Component>>,
    length: Option<i32>
}

impl WidthSpring {
    pub fn new(of: Rc<RefCell<dyn Component>>) -> Spring {
        Spring::wrap(Self { of, length: None })
    }
}

impl Debug for WidthSpring {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("WidthSpring")
            .field("of", &self.of.borrow())
            .field("length", &self.length)
            .finish()
    }
}

impl SpringImpl for WidthSpring {
    fn get_requirements(&self) -> LengthRequirements {
        self.of.borrow().get_size_requirements().width & *MAX
    }

    fn get_length(&self) -> i32 {
        self.length.unwrap_or_else(|| {
            self.get_requirements().preferred
        })
    }

    fn set_length(&mut self, width: i32) {
        self.length = Some(width);
    }

    fn is_cyclic(&self, _seen: &mut SpringSet) -> bool {
        false
    }
}

/// A spring whose requirements and length are defined by that of a
/// supplied component. The spring keeps track of changes in the
/// component.
#[derive(Clone)]
pub struct HeightSpring {
    of: Rc<RefCell<dyn Component>>,
    length: Option<i32>
}

impl HeightSpring {
    pub fn new(of: Rc<RefCell<dyn Component>>) -> Spring {
        Spring::wrap(Self { of, length: None })
    }
}

impl Debug for HeightSpring {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("HeightSpring")
            .field("of", &self.of.borrow())
            .field("length", &self.length)
            .finish()
    }
}

impl SpringImpl for HeightSpring {
    fn get_requirements(&self) -> LengthRequirements {
        self.of.borrow().get_size_requirements().height & *MAX
    }

    fn get_length(&self) -> i32 {
        self.length.unwrap_or_else(|| {
            self.get_requirements().preferred
        })
    }

    fn set_length(&mut self, height: i32) {
        self.length = Some(height);
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
        self.a.set_strain(self.get_strain(length));
        self.b.set_length(length - self.a.get_length());
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
        self.s.set_length(-length);
    }

    fn is_cyclic(&self, _seen: &mut SpringSet) -> bool {
        unimplemented!();
    }
}
