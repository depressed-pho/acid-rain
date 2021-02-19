pub mod registry;
pub use registry::*;

use std::fmt::Debug;

pub type ID = String;

pub trait Tile: Debug + Send + Sync {
    /** Return the tile ID such as "acid-rain:dirt".
     */
    fn id(&self) -> ID;
}
