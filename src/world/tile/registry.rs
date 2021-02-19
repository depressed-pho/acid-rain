use crate::world::tile::*;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

lazy_static! {
    static ref REGISTRY: RwLock<TileRegistry> = RwLock::new(TileRegistry::new());
}

pub fn get_tile_registry<'a>() -> RwLockReadGuard<'a, TileRegistry> {
    REGISTRY.read().unwrap()
}

pub fn get_tile_registry_mut<'a>() -> RwLockWriteGuard<'a, TileRegistry> {
    REGISTRY.write().unwrap()
}

/** The tile registry is a singleton that contains immutable Tile
 * objects.
 */
#[derive(Debug)]
pub struct TileRegistry {
    tiles: HashMap<ID, Box<dyn Tile>>
}

impl TileRegistry {
    fn new() -> Self {
        Self {
            tiles: HashMap::new()
        }
    }

    pub fn register(&mut self, tile: Box<dyn Tile>) -> Result<(), ConflictingTileIDError> {
        let id = tile.id();
        if let None = self.tiles.insert(id.clone(), tile) {
            Ok(())
        }
        else {
            Err(ConflictingTileIDError { id })
        }
    }
}

// Error objects
#[derive(Debug)]
pub struct ConflictingTileIDError {
    pub id: ID
}

impl Error for ConflictingTileIDError {}

impl fmt::Display for ConflictingTileIDError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Conflicting tile ID: {}", self.id)
    }
}
