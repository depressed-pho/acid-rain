use crate::world::chunk::manager::LocalChunkManager;
use rain_builtin::loader::BuiltinModuleLoader;
use rain_core::module::loader::ModuleLoader;
use rain_core::world::World;
use rain_core::world::chunk::ChunkManager;
use rain_core::world::tile::TileRegistry;
use rain_core::world::player::{Permission, Player};
use rain_core::world::position::WorldPos;
use std::collections::HashMap;
use uuid::Uuid;

/// Local world is a server-side world which is owned by a server. The
/// server accesses the world data directly.
#[derive(Debug)]
pub struct LocalWorld {
    chunks: LocalChunkManager,
    /// Players in the world. Invariant: there is exactly one root
    /// player.
    players: HashMap<Uuid, Player>,
    root: Uuid
}

impl LocalWorld {
    /// Create a new world out of thin air.
    pub fn new() -> Self {
        let mut tiles = TileRegistry::new();

        // FIXME: Load modules properly.
        let mut btl = BuiltinModuleLoader::new();
        btl.load_tiles(&mut tiles);

        // FIXME: The initial spawn point can only be determined after
        // generating the spawn chunk.
        let spawn = WorldPos::default();

        // A fresh new world has just one root player and no one else.
        let mut players = HashMap::new();
        let root        = Player::new(Permission::Root, spawn);
        let root_id     = root.uuid();
        players.insert(root_id, root);

        Self {
            chunks: LocalChunkManager::new(tiles),
            players,
            root: root_id
        }
    }
}

impl World for LocalWorld {
    fn get_chunk_manager(&self) -> &dyn ChunkManager {
        &self.chunks
    }

    fn get_root_player(&self) -> &Player {
        self.players.get(&self.root).expect("Root player exists")
    }

    fn get_player(&self, uuid: &Uuid) -> Option<&Player> {
        self.players.get(uuid)
    }
}
