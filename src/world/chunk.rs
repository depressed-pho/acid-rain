pub mod manager;

pub mod palette;
use palette::{ChunkPalette, TileIndex};

use crate::world::tile::state::{TileState, TileStateValue};
use std::sync::Arc;

/** Unlike Minecraft our chunks are only two blocks tall so we can use
 * a larger chunk size than 16*16.
 */
const CHUNK_SIZE: usize = 32;
const CHUNK_HEIGHT: usize = 2;

pub struct Chunk {
    palette: Arc<ChunkPalette>,
    tiles: [IndexedTileState; CHUNK_SIZE * CHUNK_SIZE * CHUNK_HEIGHT]
}

impl Chunk {
    /** Create a chunk filled with a single specific tile which is
     * usually acid-rain:air.
     */
    pub fn new(palette: &Arc<ChunkPalette>, fill: &TileState) -> Chunk {
        let its = IndexedTileState::from_ts(palette, fill);
        Chunk {
            palette: palette.clone(),
            tiles: [its; CHUNK_SIZE * CHUNK_SIZE * CHUNK_HEIGHT]
        }
    }
}

/** This is a variant of TileState which has TileIndex instead of Tile
 * itself. This representation is used both on disk and in memory
 * chunk data to save space.
 */
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
