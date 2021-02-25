use crate::dimension::Rectangle;
use num::Zero;
use std::convert::From;
use std::ops::Add;

/// A point representing a location in (x, y) coordinate space,
/// specified in integer precision.
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Point<T = i32> {
    pub x: T,
    pub y: T
}

impl<T> Zero for Point<T> where T: Zero {
    fn zero() -> Self {
        Self {
            x: T::zero(),
            y: T::zero()
        }
    }

    fn is_zero(&self) -> bool {
        self.x.is_zero() && self.y.is_zero()
    }
}

impl<T> Default for Point<T> where T: Zero {
    fn default() -> Self {
        Self::zero()
    }
}

impl<T> From<Rectangle<T>> for Point<T> {
    fn from(rect: Rectangle<T>) -> Self {
        rect.pos
    }
}

/// Addition of two points is defined as component-wise.
impl<T> Add for Point<T> where T: Add<Output = T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y
        }
    }
}
