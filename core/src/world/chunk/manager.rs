use async_trait::async_trait;
use crate::world::chunk::{Chunk, ChunkPos};
use std::fmt::Debug;
use std::sync::{Arc, RwLock};

/// A chunk manager is responsible for retrieving chunks in some
/// way. Client-side chunk manager would receive chunks from the
/// server (if not cached yet), while server-side one would read
/// chunks from disk or generate them on the fly.
#[async_trait]
pub trait ChunkManager: Debug {
    /// Get a chunk at a certain position. The caller can modify the
    /// chunk data however they want to, but the modification will not
    /// be automatically propagated to remote sides. If the chunk
    /// manager is a server-side one, modified chunks are auto-saved.
    async fn get(&self, pos: ChunkPos) -> Arc<RwLock<Chunk>>;
}
