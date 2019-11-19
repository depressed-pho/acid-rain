/** A point representing a location in (x, y) coordinate space,
 * specified in integer precision.
 */
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Point {
    pub x: usize,
    pub y: usize
}

impl Default for Point {
    fn default() -> Self {
        Point {
            x: 0,
            y: 0
        }
    }
}
