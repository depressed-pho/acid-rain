use crate::world::tile::{TileRegistrar};

pub trait TileLoader {
    fn load(&self, reg: TileRegistrar);
}
