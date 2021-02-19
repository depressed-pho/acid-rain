use crate::module::loader::TileLoader;
use crate::world::tile::{ID, Tile, get_tile_registry_mut};

#[derive(Debug)]
struct Dirt {}
impl Tile for Dirt {
    fn id(&self) -> ID {
        return "acid-rain:dirt".to_string();
    }
}

pub struct BuiltinTileLoader {}
impl BuiltinTileLoader {
    pub fn new() -> Self {
        Self {}
    }
}
impl TileLoader for BuiltinTileLoader {
    fn load(&mut self) {
        let mut reg = get_tile_registry_mut();

        (*reg).register(Box::new(Dirt {})).unwrap();
    }
}
