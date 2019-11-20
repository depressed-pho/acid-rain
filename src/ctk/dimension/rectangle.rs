use crate::ctk::dimension::{
    Dimension,
    Insets,
    Point
};

/** A Rectangle specifies an area in a coordinate space that is
 * enclosed by the Rectangle object's upper-left point (x,y) in the
 * coordinate space, its width, and its height.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Rectangle {
    pub pos: Point,
    pub size: Dimension
}

impl Rectangle {
    pub fn shrink(self, i: Insets) -> Self {
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

impl Default for Rectangle {
    fn default() -> Self {
        Rectangle {
            pos: Point::zero(),
            size: Dimension::zero()
        }
    }
}
