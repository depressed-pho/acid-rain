use crate::ctk::dimension::Rectangle;
use std::convert::From;
use std::ops::{Add, Sub, Mul, Div};

/** The Dimension class encapsulates the width and height of a
 * component (in integer precision) in a single struct.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Dimension {
    pub width: usize,
    pub height: usize
}

impl Dimension {
    pub fn new(width: usize, height: usize) -> Self {
        Dimension {
            width,
            height
        }
    }
}

impl From<Rectangle> for Dimension {
    fn from(rect: Rectangle) -> Self {
        Self::new(rect.width, rect.height)
    }
}

impl Default for Dimension {
    fn default() -> Self {
        Self::new(0, 0)
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
impl Mul<usize> for Dimension {
    type Output = Dimension;

    fn mul(self, rhs: usize) -> Dimension {
        Dimension {
            width: self.width * rhs,
            height: self.height * rhs
        }
    }
}
impl Mul<Dimension> for usize {
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
impl Sub<usize> for Dimension {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self {
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
impl Div<usize> for Dimension {
    type Output = Self;

    fn div(self, rhs: usize) -> Self {
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
