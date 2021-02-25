mod tile;
use tile::load_tiles;

use rain_core::module::loader::ModuleLoader;
use rain_core::world::tile::TileRegistry;

pub struct BuiltinModuleLoader {}

impl BuiltinModuleLoader {
    pub fn new() -> Self {
        Self {}
    }
}

impl ModuleLoader for BuiltinModuleLoader {
    fn load_tiles(&mut self, tiles: &mut TileRegistry) {
        load_tiles(tiles)
    }
}
