use crate::world::tile::*;
use lazy_static::lazy_static;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

lazy_static! {
    static ref REGISTRY: RwLock<TileRegistry> = RwLock::new(TileRegistry::new());
}

pub fn get_tile_registry<'a>() -> RwLockReadGuard<'a, TileRegistry> {
    REGISTRY.read().unwrap()
}

pub fn get_tile_registry_mut<'a>() -> RwLockWriteGuard<'a, TileRegistry> {
    REGISTRY.write().unwrap()
}

/// The tile registry is a singleton that contains immutable Tile
/// objects.
#[derive(Debug)]
pub struct TileRegistry {
    tiles: HashMap<Arc<str>, Arc<dyn Tile>>
}

impl TileRegistry {
    fn new() -> Self {
        Self {
            tiles: HashMap::new()
        }
    }

    pub fn register(&mut self, tile: Arc<dyn Tile>) -> Result<(), ConflictingTileIDError> {
        let id = Arc::from(tile.id());

        if self.tiles.contains_key(tile.id()) {
            Err(ConflictingTileIDError { id })
        }
        else {
            self.tiles.insert(id, tile);
            Ok(())
        }
    }

    pub fn get<K: Borrow<str>>(&self, id: K) -> Option<&Arc<dyn Tile>> {
        self.tiles.get(id.borrow())
    }
}

// Error objects
#[derive(Debug)]
pub struct ConflictingTileIDError {
    pub id: Arc<str>
}

impl Error for ConflictingTileIDError {}

impl fmt::Display for ConflictingTileIDError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Conflicting tile ID: {}", self.id)
    }
}
