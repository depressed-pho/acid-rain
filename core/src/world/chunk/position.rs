use crate::world::chunk::CHUNK_SIZE;
use crate::world::position::WorldPos;
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
        Self {
            x: wpos.x / TryInto::<i32>::try_into(CHUNK_SIZE).unwrap(),
            y: wpos.y / TryInto::<i32>::try_into(CHUNK_SIZE).unwrap()
        }
    }
}
