pub mod chunk;
pub mod tile;
pub mod player;
pub mod position;

use crate::world::player::Player;
use std::fmt::Debug;

/// There are several types of worlds:
///
/// * LocalWorld is a server-side world which is owned by a server. The
///   server accesses the world data directly.
///
/// * SemiLocalWorld is a client-side world which is owned by a
///   builtin server in a stand-alone setup. The server and the client
///   run on separate threads and communicate by message passing.
///
/// * RemoteWorld is a client-side world which is owned by a remote
///   server. The server and the client communicate on network.
///
pub trait World : Debug {
    /// Immutably borrow the root player in this world.
    fn get_root_player(&self) -> &Player;
}
