use rain_core::world::tile::{Tile, TileRegistry};
use std::sync::Arc;

#[derive(Debug)]
struct Dirt {}
impl Tile for Dirt {
    fn id(&self) -> &str {
        return "acid-rain:dirt";
    }
}

pub fn load_tiles(reg: &mut TileRegistry) {
    reg.register(Arc::new(Dirt {})).unwrap();
}
