use crate::dimension::{
    Dimension,
    Insets,
    Point
};
use num::Zero;
use std::ops::{Add, Sub};

/// A Rectangle specifies an area in a coordinate space that is
/// enclosed by the Rectangle object's upper-left point (x,y) in the
/// coordinate space, its width, and its height.
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Rectangle<T = i32> {
    pub pos: Point<T>,
    pub size: Dimension<T>
}

impl Default for Rectangle {
    fn default() -> Self {
        Rectangle {
            pos: Point::zero(),
            size: Dimension::zero()
        }
    }
}

impl<T> Rectangle<T>
where T: Add<Output = T> + Sub<Output = T> + Copy {
    pub fn shrink(self, i: Insets<T>) -> Self {
        Rectangle {
            pos: Point {
                x: self.pos.x + i.left,
                y: self.pos.y + i.top
            },
            size: Dimension {
                width: self.size.width - i.left - i.right,
                height: self.size.height - i.top - i.bottom
            }
        }
    }
}
