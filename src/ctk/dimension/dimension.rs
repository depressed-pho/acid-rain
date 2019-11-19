use crate::ctk::dimension::Rectangle;
use std::convert::From;
use std::ops::{Add, Sub, Mul, Div};

/** The Dimension class encapsulates the width and height of a
 * component (in integer precision) in a single struct.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Dimension {
    pub width: i32,
    pub height: i32
}

impl Dimension {
    pub fn is_zero(&self) -> bool {
        self.width == 0 || self.height == 0
    }
}

impl From<Rectangle> for Dimension {
    fn from(rect: Rectangle) -> Self {
        rect.size
    }
}

impl Default for Dimension {
    fn default() -> Self {
        Dimension {
            width: 0,
            height: 0
        }
    }
}

/** Addition of two dimensions is defined as component-wise.
 */
impl Add for Dimension {
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
impl Mul<i32> for Dimension {
    type Output = Dimension;

    fn mul(self, rhs: i32) -> Dimension {
        Dimension {
            width: self.width * rhs,
            height: self.height * rhs
        }
    }
}
impl Mul<Dimension> for i32 {
    type Output = Dimension;

    fn mul(self, rhs: Dimension) -> Dimension {
        rhs * self // Commutative
    }
}

/** Multiplication of two dimensions is defined as
 * component-wise. It's not like a matrix.
 */
impl Mul for Dimension {
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
impl Sub<i32> for Dimension {
    type Output = Self;

    fn sub(self, rhs: i32) -> Self {
        Dimension {
            width: self.width - rhs,
            height: self.height - rhs
        }
    }
}

/** Subtraction of two dimensions is defined as component-wise. */
impl Sub for Dimension {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Dimension {
            width: self.width - rhs.width,
            height: self.height - rhs.height
        }
    }
}

/** Scalar division. */
impl Div<i32> for Dimension {
    type Output = Self;

    fn div(self, rhs: i32) -> Self {
        Dimension {
            width: self.width / rhs,
            height: self.height / rhs
        }
    }
}

/** Division of two dimensions is defined as component-wise. */
impl Div for Dimension {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        Dimension {
            width: self.width / rhs.width,
            height: self.height / rhs.height
        }
    }
}
