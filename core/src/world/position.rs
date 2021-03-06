/// A point representing a world position in (x, y, z) coordinate space.
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct WorldPos {
    pub x: i32,
    pub y: i32,
    /// The z coordinate is either -1 or 0.
    pub z: i8
}

impl WorldPos {
    pub fn map_x(self, f: impl Fn(i32) -> i32) -> Self {
        Self {
            x: f(self.x),
            y: self.y,
            z: self.z
        }
    }

    pub fn map_y(self, f: impl Fn(i32) -> i32) -> Self {
        Self {
            x: self.x,
            y: f(self.y),
            z: self.z
        }
    }

    pub fn map_z(self, f: impl Fn(i8) -> i8) -> Self {
        Self {
            x: self.x,
            y: self.y,
            z: f(self.z)
        }
    }
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
