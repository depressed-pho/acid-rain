use crate::world::tile::{ID, Tile, get_tile_registry_mut};

#[derive(Debug)]
struct Dirt {}
impl Tile for Dirt {
    fn id(&self) -> ID {
        return "acid-rain:dirt".to_string();
    }
}

pub fn load_tiles() {
    let mut reg = get_tile_registry_mut();

    (*reg).register(Box::new(Dirt {})).unwrap();
}
