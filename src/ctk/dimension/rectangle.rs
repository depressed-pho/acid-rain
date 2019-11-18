/** A Rectangle specifies an area in a coordinate space that is
 * enclosed by the Rectangle object's upper-left point (x,y) in the
 * coordinate space, its width, and its height.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Rectangle {
    pub x: usize,
    pub y: usize,
    pub width: usize,
    pub height: usize
}

impl Default for Rectangle {
    fn default() -> Self {
        Rectangle {
            x: 0,
            y: 0,
            width: 0,
            height: 0
        }
    }
}
