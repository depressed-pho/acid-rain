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
    async fn get(&self, pos: ChunkPos) -> Arc<RwLock<Chunk>>;
}
