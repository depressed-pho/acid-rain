pub mod registry;

use std::fmt::Debug;

pub type ID<'a> = &'a str;

pub trait Tile: Debug {
    /** Return the tile ID such as "acid-rain:dirt".
     */
    fn id(&self) -> ID;
}
