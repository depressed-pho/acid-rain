mod tile;
use tile::load_tiles;

use rain_core::module::loader::ModuleLoader;

pub struct BuiltinModuleLoader {}

impl BuiltinModuleLoader {
    pub fn new() -> Self {
        BuiltinModuleLoader {}
    }
}

impl ModuleLoader for BuiltinModuleLoader {
    fn load_tiles(&mut self) {
        load_tiles()
    }
}
