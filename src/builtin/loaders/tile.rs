use crate::module::loader::TileLoader;
use crate::world::tile::{ID, Tile, TileRegistrar};

#[derive(Debug)]
struct Dirt {}
impl Tile for Dirt {
    fn id(&self) -> ID {
        return "dirt";
    }
}

pub struct BuiltinTileLoader {}
impl BuiltinTileLoader {
    pub fn new() -> Self {
        Self {}
    }
}
impl TileLoader for BuiltinTileLoader {
    fn load(&self, reg: TileRegistrar) {
        reg.register(Box::new(Dirt {}));
    }
}
