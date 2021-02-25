use crate::world::tile::Tile;
use std::sync::Arc;

pub type TileStateValue = u32;

/** TileState is a struct containing a Tile and a single integral
 * state value. The interpretation of the state value depends on the
 * corresponding tile.
 */
#[derive(Debug)]
pub struct TileState {
    pub tile: Arc<dyn Tile>,
    pub value: TileStateValue
}
