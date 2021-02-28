use crate::world::chunk::CHUNK_SIZE;
use crate::world::position::WorldPos;
use num::Integer;
use std::convert::TryInto;

/// A point representing a chunk position in (x, y) coordinate
/// space. It is computed by dividing WorldPos by [CHUNK_SIZE] and
/// rounding towards negative infinity.
#[derive(Eq, PartialEq, Clone, Copy, Hash, Debug)]
pub struct ChunkPos {
    pub x: i32,
    pub y: i32
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
