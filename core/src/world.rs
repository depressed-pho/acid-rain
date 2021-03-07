pub mod chunk;
pub mod tile;
pub mod player;
pub mod position;

use crate::world::chunk::{Chunk, ChunkPos};
use crate::world::player::Player;
use std::fmt::Debug;
use uuid::Uuid;

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
    /// Get the chunk at a certain position if it's
    /// available. Otherwise an event ChunkArrived will happen later.
    fn get_chunk(&self, pos: ChunkPos) -> Option<&Chunk>;

    // FIXME: Remove this later.
    fn ensure_chunk_exists(&mut self, pos: ChunkPos);

    /// Immutably borrow the root player in this world.
    fn get_root_player(&self) -> &Player;

    /// Immutably borrow a player in this world.
    fn get_player(&self, uuid: &Uuid) -> Option<&Player>;
}
