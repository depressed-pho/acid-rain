use crate::dimension::{Dimension, Insets};
use num::{Bounded, Zero};
use std::cmp::{min, max};
use std::ops::{BitAnd, Add, Mul};

#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct LengthRequirements<T = i32> {
    pub minimum: T,
    pub preferred: T,
    pub maximum: T
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct SizeRequirements<T = i32> {
    pub width: LengthRequirements<T>,
    pub height: LengthRequirements<T>
}

impl<T: Bounded + Zero + Copy> LengthRequirements<T> {
    pub fn any() -> Self {
        LengthRequirements {
            minimum: T::zero(),
            preferred: T::zero(),
            maximum: T::max_value()
        }
    }

    pub fn at_least(len: T) -> Self {
        LengthRequirements {
            minimum: len,
            preferred: len,
            maximum: T::max_value()
        }
    }

    pub fn exactly(len: T) -> Self {
        LengthRequirements {
            minimum: len,
            preferred: len,
            maximum: len
        }
    }

    pub fn preferred(self, len: T) -> Self {
        LengthRequirements {
            preferred: len,
            ..self
        }
    }
}

impl<T: Bounded + Zero + Copy> SizeRequirements<T> {
    pub fn any() -> Self {
        SizeRequirements {
            width: LengthRequirements::any(),
            height: LengthRequirements::any()
        }
    }

    pub fn at_least(size: Dimension<T>) -> Self {
        SizeRequirements {
            width: LengthRequirements::at_least(size.width),
            height: LengthRequirements::at_least(size.height)
        }
    }

    pub fn exactly(size: Dimension<T>) -> Self {
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
impl<T: Ord> BitAnd for LengthRequirements<T> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        LengthRequirements {
            minimum: max(self.minimum, rhs.minimum),
            preferred: max(self.preferred, rhs.preferred),
            maximum: min(self.maximum, rhs.maximum)
        }
    }
}

impl<T> BitAnd for SizeRequirements<T>
where LengthRequirements<T>: BitAnd<Output = LengthRequirements<T>> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        SizeRequirements {
            width: self.width & rhs.width,
            height: self.height & rhs.height
        }
    }
}

/** Scalar addition.
 */
impl<T> Add<T> for LengthRequirements<T> where T: Add<Output = T> + Copy {
    type Output = Self;

    fn add(self, rhs: T) -> Self {
        LengthRequirements {
            minimum: self.minimum + rhs,
            maximum: self.maximum + rhs,
            preferred: self.preferred + rhs
        }
    }
}

/** Addition of two LengthRequirements is defined as
 * component-wise.
 */
impl<T> Add for LengthRequirements<T> where T: Add<Output = T> + Copy {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        LengthRequirements {
            minimum: self.minimum + rhs.minimum,
            maximum: self.maximum + rhs.maximum,
            preferred: self.preferred + rhs.preferred
        }
    }
}

/** Scalar multiplication.
 */
impl<T> Mul<T> for LengthRequirements<T> where T: Mul<Output = T> + Copy {
    type Output = Self;

    fn mul(self, rhs: T) -> Self {
        LengthRequirements {
            minimum: self.minimum * rhs,
            maximum: self.maximum * rhs,
            preferred: self.preferred * rhs
        }
    }
}

/** Addition of SizeRequirements and Dimension is defined as
 * component-wise.
 */
impl<T> Add<Dimension<T>> for SizeRequirements<T>
where LengthRequirements<T>: Add<T, Output = LengthRequirements<T>> {
    type Output = Self;

    fn add(self, rhs: Dimension<T>) -> Self {
        SizeRequirements {
            width: self.width + rhs.width,
            height: self.height + rhs.height
        }
    }
}

/** Addition of SizeRequirements and Insets is defined as
 * component-wise.
 */
impl<T> Add<Insets<T>> for SizeRequirements<T>
where LengthRequirements<T>: Add<T, Output = LengthRequirements<T>> {
    type Output = Self;

    fn add(self, rhs: Insets<T>) -> Self {
        SizeRequirements {
            width: self.width + rhs.left + rhs.right,
            height: self.height + rhs.top + rhs.bottom
        }
    }
}

/** Multiplication of SizeRequirements and Dimension is defined as
 * component-wise. It's not like a matrix.
 */
impl<T> Mul<Dimension<T>> for SizeRequirements<T>
where LengthRequirements<T>: Mul<T, Output = LengthRequirements<T>> {
    type Output = Self;

    fn mul(self, rhs: Dimension<T>) -> Self {
        SizeRequirements {
            width: self.width * rhs.width,
            height: self.height * rhs.height
        }
    }
}
