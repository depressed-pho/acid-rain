use crate::world::chunk::CHUNK_SIZE;
use crate::world::position::WorldPos;
use num_integer::Integer;
use std::convert::TryInto;

/// A point representing a chunk position in (x, y) coordinate
/// space. It is computed by dividing WorldPos by [CHUNK_SIZE] and
/// rounding towards negative infinity.
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct ChunkPos {
    pub x: i32,
    pub y: i32
}

impl ChunkPos {
    pub fn map_x(self, f: impl Fn(i32) -> i32) -> Self {
        Self {
            x: f(self.x),
            y: self.y,
        }
    }

    pub fn map_y(self, f: impl Fn(i32) -> i32) -> Self {
        Self {
            x: self.x,
            y: f(self.y),
        }
    }


    /// [WorldPos] does not implement From<ChunkPos> because
    /// [ChunkPos] has no `z` component.
    pub fn into_wpos(self, z: i8) -> WorldPos {
        let chunk_size: i32 = CHUNK_SIZE.try_into().unwrap();
        WorldPos {
            x: self.x * chunk_size,
            y: self.y * chunk_size,
            z: z
        }
    }
}

impl Default for ChunkPos {
    fn default() -> Self {
        Self {
            x: 0,
            y: 0
        }
    }
}

impl From<WorldPos> for ChunkPos {
    fn from(wpos: WorldPos) -> Self {
        let chunk_size: i32 = CHUNK_SIZE.try_into().unwrap();
        Self {
            x: wpos.x.div_floor(&chunk_size),
            y: wpos.y.div_floor(&chunk_size)
        }
    }
}
