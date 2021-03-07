use rain_core::world::chunk::{Chunk, ChunkPos, ChunkManager};
use rain_core::world::chunk::palette::ChunkPalette;
use rain_core::world::tile::{ArcTile, TileRegistry};
use std::collections::HashMap;
use std::sync::Arc;

/// This is a server-side chunk manager. When a chunk is requested, it
/// searches for the chunk in the loaded chunk map, or loads the chunk
/// from disk, or performs a chunk generation. The server thread
/// periodically asks the manager to tick and save chunks.
///
/// If a chunk has no players nearby, and is not anchored by any tile
/// entities, the chunk will be unloaded from memory. The cache is
/// therefore not an LRU.
///
#[derive(Debug)]
pub struct LocalChunkManager {
    tiles: Arc<TileRegistry>,
    palette: Arc<ChunkPalette>, // Shared among chunks.
    loaded: HashMap<ChunkPos, Chunk>
}

impl LocalChunkManager {
    pub fn new(tiles: TileRegistry) -> Self {
        let mut palette = ChunkPalette::new();

        // FIXME: Construct a chunk palette properly.
        let dirt = tiles.get("acid-rain:dirt").unwrap();
        palette.insert(dirt.id());

        Self {
            tiles: Arc::new(tiles),
            palette: Arc::new(palette),
            loaded: HashMap::new()
        }
    }

    fn generate(&self, _pos: ChunkPos) -> Chunk {
        // FIXME: Generate a chunk properly.
        let dirt = self.tiles.get("acid-rain:dirt").unwrap();
        let dirt_ts = dirt.default_state();

        Chunk::new(&self.tiles, &self.palette, &dirt_ts)
    }
}

impl ChunkManager for LocalChunkManager {
    fn get(&self, pos: ChunkPos) -> Option<&Chunk> {
        self.loaded.get(&pos)
    }

    fn ensure_chunk_exists(&mut self, pos: ChunkPos) {
        if !self.loaded.contains_key(&pos) {
            let chunk = self.generate(pos);
            self.loaded.insert(pos, chunk);
        }
    }
}
