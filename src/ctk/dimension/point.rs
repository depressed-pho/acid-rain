use crate::ctk::dimension::Rectangle;
use std::convert::From;

/** A point representing a location in (x, y) coordinate space,
 * specified in integer precision.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Point {
    pub x: i32,
    pub y: i32
}

impl Default for Point {
    fn default() -> Self {
        Point {
            x: 0,
            y: 0
        }
    }
}

impl From<Rectangle> for Point {
    fn from(rect: Rectangle) -> Self {
        rect.pos
    }
}
