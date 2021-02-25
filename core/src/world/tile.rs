pub mod registry;
pub use registry::*;

pub mod state;
pub use state::*;

use std::fmt::Debug;
use std::sync::Arc;

pub trait Tile: Debug + Send + Sync {
    /// Return the tile ID such as `acid-rain:dirt`.
    fn id(&self) -> &str;

    /// Return the default state value of the tile.
    fn default_state_value(&self) -> TileStateValue {
        0
    }
}

/// This trait is a workaround for the issue explained in [&Rc, &Arc
/// as method receivers? - Rust
/// Internals](https://internals.rust-lang.org/t/rc-arc-as-method-receivers/11069/9)
pub trait ArcTile {
    fn default_state(self: &Self) -> TileState;
}

impl ArcTile for Arc<dyn Tile> {
    fn default_state(self: &Arc<dyn Tile>) -> TileState {
        TileState {
            tile: self.clone(),
            value: self.default_state_value()
        }
    }
}
