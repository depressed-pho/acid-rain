mod manager;
pub use manager::*;

mod offset;
pub use offset::*;

mod position;
pub use position::*;

pub mod palette;
use palette::{ChunkPalette, TileIndex};

use crate::world::tile::{TileRegistry};
use crate::world::tile::state::{TileState, TileStateValue};
use std::borrow::Borrow;
use std::fmt as fmt;
use std::sync::Arc;

/// Unlike Minecraft our chunks are only two blocks tall so we can use
/// a larger chunk size than 16*16.
pub const CHUNK_SIZE: usize = 32;
pub const CHUNK_HEIGHT: usize = 2;

pub struct Chunk {
    treg: Arc<TileRegistry>,
    palette: Arc<ChunkPalette>,
    tiles: [IndexedTileState; CHUNK_SIZE * CHUNK_SIZE * CHUNK_HEIGHT]
}

impl Chunk {
    /// Create a chunk filled with a single specific tile which is
    /// usually `acid-rain:air`.
    pub fn new(treg: &Arc<TileRegistry>, palette: &Arc<ChunkPalette>, fill: &TileState) -> Chunk {
        let its = IndexedTileState::from_ts(palette, fill);
        Chunk {
            treg: treg.clone(),
            palette: palette.clone(),
            tiles: [its; CHUNK_SIZE * CHUNK_SIZE * CHUNK_HEIGHT]
        }
    }

    pub fn get_tile_state(&self, off: &TileOffset) -> TileState {
        assert!(off.x < CHUNK_SIZE);
        assert!(off.y < CHUNK_SIZE);
        assert!(off.z < CHUNK_HEIGHT);
        let its  = self.tiles[off.z * CHUNK_HEIGHT * CHUNK_SIZE + off.y * CHUNK_SIZE + off.x];
        let id   = self.palette.id_of(&its.index).expect("Unknown tile index");
        let tile = self.treg.get(id.borrow()).expect("Unknown tile");
        TileState {
            tile: tile.clone(),
            value: its.value
        }
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Chunk")
         .finish()
    }
}

/// This is a variant of TileState which has TileIndex instead of Tile
/// itself. This representation is used both on disk and in memory
/// chunk data to save space.
#[derive(Copy, Clone, Debug)]
struct IndexedTileState {
    index: TileIndex,
    value: TileStateValue
}

impl IndexedTileState {
    fn from_ts(palette: &Arc<ChunkPalette>, ts: &TileState) -> Self {
        let index = {
            if let Some(n) = palette.index_of(&ts.tile.id()) {
                n
            }
            else {
                panic!("No index in the chunk palette: {:?}", ts)
            }
        };
        Self {
            index,
            value: ts.value
        }
    }
}
