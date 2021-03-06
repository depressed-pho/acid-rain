use async_trait::async_trait;
use rain_core::world::chunk::{Chunk, ChunkPos, ChunkManager};
use rain_core::world::chunk::palette::ChunkPalette;
use rain_core::world::tile::{ArcTile, TileRegistry};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug)]
/// This is a server-side chunk manager. When a chunk is requested, it
/// searches for the chunk in the loaded chunk map, or loads the chunk
/// from disk, or performs a chunk generation. The server thread
/// periodically asks the manager to tick and save chunks.
///
/// If a chunk has no players nearby, and is not anchored by any tile
/// entities, the chunk will be unloaded from memory. The cache is
/// therefore not an LRU.
///
pub struct LocalChunkManager {
    tiles: TileRegistry,
    palette: Arc<ChunkPalette>, // Shared among chunks.
    loaded: RwLock<HashMap<ChunkPos, Arc<RwLock<Chunk>>>>
}

impl LocalChunkManager {
    pub fn new(tiles: TileRegistry) -> Self {
        let mut palette = ChunkPalette::new();

        // FIXME: Construct a chunk palette properly.
        let dirt = tiles.get("acid-rain:dirt").unwrap();
        palette.insert(dirt.id());

        Self {
            tiles,
            palette: Arc::new(palette),
            loaded: RwLock::new(HashMap::new())
        }
    }

    async fn generate(&self, _pos: ChunkPos) -> Chunk {
        // FIXME: Generate a chunk properly.
        let dirt = self.tiles.get("acid-rain:dirt").unwrap();
        let dirt_ts = dirt.default_state();

        Chunk::new(&self.palette, &dirt_ts)
    }
}

#[async_trait]
impl ChunkManager for LocalChunkManager {
    async fn get(&self, pos: ChunkPos) -> Arc<RwLock<Chunk>> {
        if let Some(chunk) = self.loaded.read().await.get(&pos) {
            return chunk.clone();
        }
        let chunk = Arc::new(RwLock::new(self.generate(pos).await));
        self.loaded.write().await.insert(pos, chunk.clone());
        chunk
    }
}
