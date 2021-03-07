use crate::dimension::{Dimension, Insets};
use num_traits::{
    Bounded,
    Signed,
    Zero,
    CheckedAdd,
    CheckedMul
};
use std::cmp::{min, max};
use std::ops::{BitAnd, Add, Mul, Neg};

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

    pub fn at_most(len: T) -> Self {
        LengthRequirements {
            minimum: T::zero(),
            preferred: T::zero(),
            maximum: len
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

pub(crate) fn checked_add<T: Bounded + CheckedAdd>(a: T, b: T) -> T {
    if let Some(c) = a.checked_add(&b) {
        c
    }
    else {
        T::max_value()
    }
}

pub(crate) fn checked_mul<T: Signed + Bounded + CheckedMul>(a: T, b: T) -> T {
    if let Some(c) = a.checked_mul(&b) {
        c
    }
    else {
        if a.is_positive() {
            if b.is_positive() { T::max_value() }
            else               { T::min_value() }
        }
        else {
            if b.is_positive() { T::min_value() }
            else               { T::max_value() }
        }
    }
}

/// Combine two requirements. It follows the following law:
///
/// ∀r:LengthRequirements. any() & r == r & any() == r
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

/// Scalar addition.
impl<T> Add<T> for LengthRequirements<T>
where T: Add<Output = T> + Signed + Bounded + CheckedAdd + Copy {
    type Output = Self;

    fn add(self, rhs: T) -> Self {
        LengthRequirements {
            minimum: checked_add(self.minimum, rhs),
            maximum: checked_add(self.maximum, rhs),
            preferred: checked_add(self.preferred, rhs)
        }
    }
}

/// Addition of two LengthRequirements is defined as
/// component-wise.
impl<T> Add for LengthRequirements<T>
where T: Add<Output = T> + Signed + Bounded + CheckedAdd + Copy {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        LengthRequirements {
            minimum: checked_add(self.minimum, rhs.minimum),
            maximum: checked_add(self.maximum, rhs.maximum),
            preferred: checked_add(self.preferred, rhs.preferred)
        }
    }
}

/// Scalar multiplication.
impl<T> Mul<T> for LengthRequirements<T>
where T: Mul<Output = T> + Signed + Bounded + CheckedMul + Copy {
    type Output = Self;

    fn mul(self, rhs: T) -> Self {
        LengthRequirements {
            minimum: checked_mul(self.minimum, rhs),
            maximum: checked_mul(self.maximum, rhs),
            preferred: checked_mul(self.preferred, rhs)
        }
    }
}

/// A LengthRequirements can be seen as a pair of intervals with a
/// single common point: the preferred length. The negation of an
/// interval [a, b] can be defined as [-b, -a] because a <= b.
impl<T> Neg for LengthRequirements<T> where T: Neg<Output = T> {
    type Output = Self;

    fn neg(self) -> Self {
        LengthRequirements {
            minimum: -self.maximum,
            maximum: -self.minimum,
            preferred: -self.preferred
        }
    }
}

/// Addition of SizeRequirements and Dimension is defined as
/// component-wise.
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

/// Addition of SizeRequirements and Insets is defined as
/// component-wise.
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

/// Multiplication of SizeRequirements and Dimension is defined as
/// component-wise. It's not like a matrix.
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
