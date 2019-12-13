use crate::dimension::Rectangle;
use num::Zero;
use std::convert::From;
use std::ops::{Add, Sub, Mul, Div};

/** The Dimension class encapsulates the width and height of a
 * component (in integer precision) in a single struct.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Dimension<T = i32> {
    pub width: T,
    pub height: T
}

impl<T> Dimension<T> where T: Mul<Output = T> + Copy {
    pub fn area(&self) -> T {
        self.width * self.height
    }
}

impl<T> Zero for Dimension<T> where T: Zero {
    fn zero() -> Self {
        Dimension {
            width: T::zero(),
            height: T::zero()
        }
    }

    fn is_zero(&self) -> bool {
        // Intentionally || and not &&.
        self.width.is_zero() || self.height.is_zero()
    }
}

impl<T> From<Rectangle<T>> for Dimension<T> {
    fn from(rect: Rectangle<T>) -> Self {
        rect.size
    }
}

impl<T> Default for Dimension<T> where T: Zero {
    fn default() -> Self {
        Self::zero()
    }
}

/** Addition of two dimensions is defined as component-wise.
 */
impl<T> Add for Dimension<T> where T: Add<Output = T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Dimension {
            width: self.width + rhs.width,
            height: self.height + rhs.height
        }
    }
}

/** Scalar multiplication. Not general at all. See also
 * https://github.com/rust-lang/rfcs/issues/2608
 */
impl<T> Mul<T> for Dimension<T> where T: Mul<Output = T> + Copy {
    type Output = Self;

    fn mul(self, rhs: T) -> Self {
        Dimension {
            width: self.width * rhs,
            height: self.height * rhs
        }
    }
}
impl Mul<Dimension<i32>> for i32 {
    type Output = Dimension<i32>;

    fn mul(self, rhs: Dimension<i32>) -> Dimension<i32> {
        rhs * self // Commutative
    }
}

/** Multiplication of two dimensions is defined as
 * component-wise. It's not like a matrix.
 */
impl<T> Mul for Dimension<T> where T: Mul<Output = T> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        Dimension {
            width: self.width * rhs.width,
            height: self.height * rhs.height
        }
    }
}

/** Scalar subtraction.
 */
impl<T> Sub<T> for Dimension<T> where T: Sub<Output = T> + Copy {
    type Output = Self;

    fn sub(self, rhs: T) -> Self {
        Dimension {
            width: self.width - rhs,
            height: self.height - rhs
        }
    }
}

/** Subtraction of two dimensions is defined as component-wise. */
impl<T> Sub for Dimension<T> where T: Sub<Output = T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Dimension {
            width: self.width - rhs.width,
            height: self.height - rhs.height
        }
    }
}

/** Scalar division. */
impl<T> Div<T> for Dimension<T> where T: Div<Output = T> + Copy {
    type Output = Self;

    fn div(self, rhs: T) -> Self {
        Dimension {
            width: self.width / rhs,
            height: self.height / rhs
        }
    }
}

/** Division of two dimensions is defined as component-wise. */
impl<T> Div for Dimension<T> where T: Div<Output = T> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        Dimension {
            width: self.width / rhs.width,
            height: self.height / rhs.height
        }
    }
}
