use crate::ctk::dimension::Rectangle;
use std::convert::From;
use std::ops::Add;

/** A point representing a location in (x, y) coordinate space,
 * specified in integer precision.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Point {
    pub x: i32,
    pub y: i32
}

impl Point {
    pub fn zero() -> Self {
        Point {
            x: 0,
            y: 0
        }
    }

    pub fn is_zero(&self) -> bool {
        self.x == 0 && self.y == 0
    }
}

impl Default for Point {
    fn default() -> Self {
        Self::zero()
    }
}

impl From<Rectangle> for Point {
    fn from(rect: Rectangle) -> Self {
        rect.pos
    }
}

/** Addition of two points is defined as component-wise.
 */
impl Add for Point {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y
        }
    }
}
