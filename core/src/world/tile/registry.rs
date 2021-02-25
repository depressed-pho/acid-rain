use crate::world::tile::*;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

/// The tile registry is struct that contains immutable Tile
/// objects. It is constructed while loading a world, and becomes
/// immutable afterwards.
#[derive(Debug)]
pub struct TileRegistry {
    tiles: HashMap<Arc<str>, Arc<dyn Tile>>
}

impl TileRegistry {
    pub fn new() -> Self {
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
