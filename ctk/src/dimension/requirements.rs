use crate::dimension::Dimension;
use std::cmp::{min, max};
use std::ops::BitAnd;

#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct LengthRequirements {
    pub minimum: i32,
    pub preferred: i32,
    pub maximum: i32
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct SizeRequirements {
    pub width: LengthRequirements,
    pub height: LengthRequirements
}

impl LengthRequirements {
    pub fn any() -> Self {
        LengthRequirements {
            minimum: 0,
            preferred: 0,
            maximum: i32::max_value()
        }
    }

    pub fn at_least(len: i32) -> Self {
        LengthRequirements {
            minimum: len,
            preferred: len,
            maximum: i32::max_value()
        }
    }

    pub fn exactly(len: i32) -> Self {
        LengthRequirements {
            minimum: len,
            preferred: len,
            maximum: len
        }
    }
}

impl SizeRequirements {
    pub fn any() -> Self {
        SizeRequirements {
            width: LengthRequirements::any(),
            height: LengthRequirements::any()
        }
    }

    pub fn at_least(size: Dimension) -> Self {
        SizeRequirements {
            width: LengthRequirements::at_least(size.width),
            height: LengthRequirements::at_least(size.height)
        }
    }

    pub fn exactly(size: Dimension) -> Self {
        SizeRequirements {
            width: LengthRequirements::exactly(size.width),
            height: LengthRequirements::exactly(size.height)
        }
    }
}

/** Combine two requirements. It follows the following law:
 *
 * ∀r:LengthRequirements. any() & r == r & any() == r
 */
impl BitAnd for LengthRequirements {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        LengthRequirements {
            minimum: max(self.minimum, rhs.minimum),
            preferred: max(self.preferred, rhs.preferred),
            maximum: min(self.maximum, rhs.maximum)
        }
    }
}

impl BitAnd for SizeRequirements {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        SizeRequirements {
            width: self.width & rhs.width,
            height: self.height & rhs.height
        }
    }
}
