use std::borrow::Borrow;
use std::collections::{HashMap, BTreeMap};
use std::sync::Arc;

pub type TileIndex = u32;

/** A chunk palette is a bidirectional map between tile ID from/to
 * numerical tile index. It
 * is used for compressing chunk data both in memory and on disk. The
 * same palette is shared between all the chunks in a world.
 *
 * A palette saved on disk may contain tiles that no longer
 * exist. When we load a palette and find some tiles are missing, we
 * display a warning about that. And when we load a chunk which
 * contains missing tiles, we replace them with acid-rain:air or
 * something equivalently convincing.
 */
#[derive(Debug)]
pub struct ChunkPalette {
    index_of_: HashMap<Arc<str>, TileIndex>,
    id_of_: BTreeMap<TileIndex, Arc<str>>
}

impl ChunkPalette {
    pub fn new() -> Self {
        Self {
            index_of_: HashMap::new(),
            id_of_: BTreeMap::new()
        }
    }

    /** Insert a tile ID to the palette. Inserting the same ID twice
     * is not an error. It's just ignored.
     */
    pub fn insert<K: Borrow<str>>(&mut self, id: K) {
        if !self.index_of_.contains_key(id.borrow()) {
            let arc_id = Arc::from(id.borrow());
            let new_index = {
                // BTreeMap.last_entry() is experimental. See
                // https://github.com/rust-lang/rust/issues/62924
                if let Some((max_index, _)) = self.id_of_.iter().next_back() {
                    max_index + 1
                }
                else {
                    0
                }
            };
            self.index_of_.insert(Arc::clone(&arc_id), new_index);
            self.id_of_.insert(new_index, arc_id);
        }
    }

    pub fn index_of<K: Borrow<str>>(&self, tile: &K) -> Option<TileIndex> {
        self.index_of_.get(tile.borrow()).copied()
    }
}
