use crate::world::position::WorldPos;
use uuid::Uuid;

#[derive(Debug)]
pub enum Permission {
    /// Each world has exactly one root player. The root player is the
    /// owner of the world, and can use any operations including cheat
    /// commands. When a player enters a single-player world, the
    /// player becomes the root regardless of the player identity.
    Root,

    /// Administrators can use any operations including cheat commands
    /// and op'ing and deop'ing any players.
    Administrator,

    /// Members can play the game as normal, but cannot use certain
    /// op-only commands.
    Member,

    /// Visitors cannot break tiles or pick items. They can still
    /// interact with tiles though.
    Visitor
}

#[derive(Debug)]
pub struct Player {
    uuid: Uuid,
    perm: Permission,
    pos: WorldPos
}

impl Player {
    /// Create a new player with a given permission, position, and a
    /// random UUID.
    pub fn new(perm: Permission, pos: WorldPos) -> Self {
        Self {
            uuid: Uuid::new_v4(),
            perm,
            pos
        }
    }

    pub fn uuid(&self) -> Uuid {
        self.uuid
    }
}
