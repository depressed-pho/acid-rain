use crate::world::chunk::{CHUNK_SIZE, CHUNK_HEIGHT};
use crate::world::position::WorldPos;
use num_integer::Integer;
use std::convert::TryInto;

/// This is an offset to a tile in a chunk, convertible from WorldPos.
#[derive(Copy, Clone, Debug)]
pub struct TileOffset {
    pub x: usize,
    pub y: usize,
    pub z: usize
}

impl From<WorldPos> for TileOffset {
    fn from(wpos: WorldPos) -> Self {
        let chunk_size: i32 = CHUNK_SIZE.try_into().unwrap();
        let chunk_height: i8 = CHUNK_HEIGHT.try_into().unwrap();
        Self {
            x: wpos.x.mod_floor(&chunk_size) as usize,
            y: wpos.y.mod_floor(&chunk_size) as usize,
            z: wpos.z.mod_floor(&chunk_height) as usize
        }
    }
}
