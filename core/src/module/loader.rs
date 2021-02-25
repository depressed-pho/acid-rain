use crate::world::tile::TileRegistry;

pub trait ModuleLoader {
    fn load_tiles(&mut self, tiles: &mut TileRegistry);
}
