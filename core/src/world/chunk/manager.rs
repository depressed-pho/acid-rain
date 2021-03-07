use crate::world::chunk::{Chunk, ChunkPos};
use std::fmt::Debug;

/// A chunk manager is responsible for retrieving chunks in some
/// way. Client-side chunk manager would receive chunks from the
/// server (if not cached yet), while server-side one would read
/// chunks from disk or generate them on the fly.
pub trait ChunkManager: Debug {
    /// Immutably borrow the chunk at a certain position if it's
    /// available.
    fn get(&self, pos: ChunkPos) -> Option<&Chunk>;

    // FIXME: Remove this later.
    fn ensure_chunk_exists(&mut self, pos: ChunkPos);
}
