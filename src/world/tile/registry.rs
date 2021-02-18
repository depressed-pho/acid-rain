use crate::module as Module;
use crate::world::tile::*;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::RwLock;

lazy_static! {
    static ref REGISTRY: RwLock<TileRegistry> = RwLock::new(TileRegistry::new());
}

/** The tile registry is a singleton that contains immutable Tile
 * objects.
 */
#[derive(Debug)]
pub struct TileRegistry {
    root: HashMap<Module::ID, HashMap<ID, Box<dyn Tile>>>
}

impl TileRegistry {
    fn new() -> Self {
        Self {
            root: HashMap::new()
        }
    }

    fn register(&mut self, module: Module::ID, tile: Box<dyn Tile>) -> &mut Self {
        if let Some(tiles) = self.root.get_mut(module) {
            tiles.insert(tile.id(), tile);
        }
        else {
            let mut tiles = HashMap::new();
            tiles.insert(tile.id(), tile);
            self.root.insert(module, tiles);
        }
        self
    }
}

/** A proxy object that registers tiles in a certain module.
 */
pub struct TileRegistrar {
    module: Module::ID
}

impl<'a> TileRegistrar {
    pub fn new(module: Module::ID) -> Self {
        Self {
            module
        }
    }

    pub fn register(&self, tile: Box<dyn Tile>) -> &Self {
        let mut reg = REGISTRY.write().unwrap();
        (*reg).register(self.module, tile);
        self
    }
}
