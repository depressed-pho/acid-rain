use async_trait::async_trait;
use rain_core::world::chunk::{Chunk, ChunkPos, ChunkManager};
use rain_core::world::chunk::palette::ChunkPalette;
use rain_core::world::tile::{ArcTile, TileRegistry};
use std::sync::{Arc, RwLock};

#[derive(Debug)]
pub struct LocalChunkManager {
    tiles: Arc<TileRegistry>,
    palette: Arc<ChunkPalette>
}

impl LocalChunkManager {
    pub fn new(tiles: Arc<TileRegistry>) -> Self {
        let mut palette = ChunkPalette::new();

        // FIXME
        let dirt = tiles.get("acid-rain:dirt").unwrap();
        palette.insert(dirt.id());

        Self {
            tiles,
            palette: Arc::new(palette)
        }
    }
}

#[async_trait]
impl ChunkManager for LocalChunkManager {
    async fn get(&self, _pos: ChunkPos) -> Arc<RwLock<Chunk>> {
        // FIXME
        let dirt = self.tiles.get("acid-rain:dirt").unwrap();
        let dirt_ts = dirt.default_state();

        Arc::new(RwLock::new(Chunk::new(&self.palette, &dirt_ts)))
    }
}
