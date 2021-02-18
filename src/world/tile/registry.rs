use crate::world::tile::*;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::RwLock;

lazy_static! {
    /** The tile registry is a singleton that contains immutable Tile
     * objects.
     */
    static ref REGISTRY: RwLock<HashMap<&'static str, Box<dyn Tile + Send + Sync>>> =
        RwLock::new(HashMap::new());
}
