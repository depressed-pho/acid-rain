use crate::ctk::dimension::{
    Dimension,
    Point,
    Rectangle
};
use super::Spring;

pub struct Constraints {
    bounds: Rectangle<Option<Box<dyn Spring>>>,
}

impl Constraints {
    /** Create an empty constraints object.
     */
    pub fn new() -> Self {
        Constraints {
            bounds: Rectangle {
                pos: Point {
                    x: None,
                    y: None
                },
                size: Dimension {
                    width: None,
                    height: None
                }
            }
        }
    }
}
