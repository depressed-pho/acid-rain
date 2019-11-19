use crate::ctk::dimension::{
    Dimension,
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

impl Default for Rectangle {
    fn default() -> Self {
        Rectangle {
            pos: Point { x: 0, y: 0 },
            size: Dimension { width: 0, height: 0 }
        }
    }
}
