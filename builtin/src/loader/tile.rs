use core::world::tile::{Tile, get_tile_registry_mut};
use std::sync::Arc;

#[derive(Debug)]
struct Dirt {}
impl Tile for Dirt {
    fn id(&self) -> &str {
        return "acid-rain:dirt";
    }
}

pub fn load_tiles() {
    let mut reg = get_tile_registry_mut();

    (*reg).register(Arc::new(Dirt {})).unwrap();
}
