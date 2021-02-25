use crate::world::chunk::manager::LocalChunkManager;
use rain_builtin::loader::BuiltinModuleLoader;
use rain_core::module::loader::ModuleLoader;
use rain_core::world::World;
use rain_core::world::tile::TileRegistry;
use std::sync::Arc;

#[derive(Debug)]
pub struct LocalWorld {
    chunks: LocalChunkManager
}

impl LocalWorld {
    pub fn new() -> Self {
        let mut tiles = TileRegistry::new();

        let mut btl = BuiltinModuleLoader::new();
        btl.load_tiles(&mut tiles);

        Self {
            chunks: LocalChunkManager::new(Arc::new(tiles))
        }
    }
}

impl World for LocalWorld {
}
