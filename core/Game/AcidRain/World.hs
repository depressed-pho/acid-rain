{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World
  ( World(..)
  , UnknownPlayerIDException(..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Player (Player, PlayerID)


-- | There are several types of worlds:
--
-- * 'LocalWorld' is a server-side world which is owned by a
--   server. The server accesses the world data directly.
--
-- * 'SemiLocalWorld' is a client-side world which is owned by a
--   builtin server in a stand-alone setup. The server and the client
--   run on separate threads and communicate by message passing.
--
-- * 'RemoteWorld' is a client-side world which is owned by a remote
--   server. The server and the client communicate on network.
--
class World α where
  -- | Lookup a chunk at a certain position if it's available. This
  -- does not block. If the chunk isn't available yet, an event
  -- ChunkArrived will fire later.
  lookupChunk ∷ MonadIO μ ⇒ ChunkPos → α → μ (Maybe Chunk)
  -- FIXME: Remove this later.
  ensureChunkExists ∷ MonadIO μ ⇒ ChunkPos → α → μ ()
  -- | Get the root player of the world.
  getRootPlayer ∷ MonadIO μ ⇒ α → μ Player
  -- | Get a player in the world having a given ID.
  getPlayer ∷ (MonadIO μ, MonadThrow μ) ⇒ PlayerID → α → μ Player

-- | An exception to be thrown when there was no player having the
-- given ID.
data UnknownPlayerIDException = UnknownPlayerIDException PlayerID
  deriving Show

instance Exception UnknownPlayerIDException
