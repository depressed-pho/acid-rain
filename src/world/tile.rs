pub mod registry;
pub use registry::*;

use std::fmt::Debug;

pub type ID = &'static str;

pub trait Tile: Debug + Send + Sync {
    /** Return the tile ID such as "dirt" in a certain module.
     */
    fn id(&self) -> ID;
}
