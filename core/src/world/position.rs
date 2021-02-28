/// A point representing a world position in (x, y, z) coordinate space.
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct WorldPos {
    pub x: i32,
    pub y: i32,
    /// The z coordinate is either -1 or 0.
    pub z: i8
}

impl Default for WorldPos {
    fn default() -> Self {
        Self {
            x: 0,
            y: 0,
            z: 0
        }
    }
}
