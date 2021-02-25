use async_trait::async_trait;
use rain_core::world::chunk::{Chunk, ChunkPos, ChunkManager};
use rain_core::world::chunk::palette::ChunkPalette;
use rain_core::world::tile::{ArcTile, get_tile_registry};
use std::sync::{Arc, RwLock};

#[derive(Debug)]
pub struct ServerChunkManager {
    palette: Arc<ChunkPalette>
}

impl ServerChunkManager {
    pub fn new() -> Self {
        let mut palette = ChunkPalette::new();

        // FIXME
        let reg = get_tile_registry();
        let dirt = reg.get("acid-rain:dirt").unwrap();
        palette.insert(dirt.id());

        Self {
            palette: Arc::new(palette)
        }
    }
}

#[async_trait]
impl ChunkManager for ServerChunkManager {
    async fn get(&self, _pos: ChunkPos) -> Arc<RwLock<Chunk>> {
        // FIXME
        let reg = get_tile_registry();
        let dirt = reg.get("acid-rain:dirt").unwrap();
        let dirt_ts = dirt.default_state();

        Arc::new(RwLock::new(Chunk::new(&self.palette, &dirt_ts)))
    }
}
