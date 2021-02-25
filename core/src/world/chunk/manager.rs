use std::fmt::Debug;

/** A chunk manager is responsible for retrieving chunks in some
 * way. Client-side chunk manager would receive chunks from the server
 * (if not cached yet), while server-side one would read chunks from
 * disk or generate them on the fly.
 */
trait ChunkManager: Debug {
}
