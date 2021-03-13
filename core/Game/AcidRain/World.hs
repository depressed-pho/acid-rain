{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World
  ( World(..)
  , WorldMode(..)
  , WorldState(..)
  , WorldNotRunningException(..)
  , UnknownPlayerIDException(..)
  ) where

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Player (Player, PlayerID)
import Prelude.Unicode ((∘))


-- | There are several types of worlds:
--
-- * 'LocalWorld' is a server-side world which is owned by a
--   server. The server accesses the world data directly. It is used
--   both in single player mode and multi player mode. In single
--   player mode the server and the client run on separate threads.
--
-- * 'RemoteWorld' is a client-side world which is owned by a remote
--   server. The server and the client communicate on network.
--
class World α where
  -- | The type of the running state of this world. It's usually an
  -- opaque type.
  type RunningStateT α ∷ *
  -- | Get the state of the world. State changes are also reported via
  -- WorldStateChanged events. Unless explicitly stated, most of the
  -- other methods of this class throws exceptions when invoked at a
  -- wrong state.
  getWorldState ∷ MonadIO μ ⇒ α → μ (WorldState (RunningStateT α))
  -- | Lookup a chunk at a certain position if it's available. This
  -- does not block. If the chunk isn't available yet, an event
  -- ChunkArrived will fire later.
  lookupChunk ∷ MonadIO μ ⇒ ChunkPos → α → μ (Maybe Chunk)
  -- FIXME: Remove this later.
  ensureChunkExists ∷ MonadIO μ ⇒ ChunkPos → α → μ ()
  -- | Get a player in the world having a given ID.
  getPlayer ∷ (MonadIO μ, MonadThrow μ) ⇒ PlayerID → α → μ Player

data WorldMode
  = SinglePlayer
  | MultiPlayer
  deriving (Show, Eq)

data WorldState rs
  = -- | The world is being loaded.
    Loading -- THINKME: progress
    -- | Concerns have been raised while loading the world. Players
    -- may choose to abort or continue loading the world, but until
    -- that the world waits for their input.
  | LoadPending -- FIXME: reason
    -- | Problems have arised while loading the world.
  | LoadFailed SomeException
    -- | The world is running.
  | Running !rs
    -- | The world has been closed.
  | Closed !(Maybe SomeException)
  deriving Show

-- | An exception to be thrown when a certain operation assuming the
-- world is running is attempted, but it was actually not running.
data WorldNotRunningException where
  WorldNotRunningException ∷ Show rs ⇒ WorldState rs → WorldNotRunningException

instance Show WorldNotRunningException where
  -- GHC can't derive Show for this, but why?
  showsPrec d (WorldNotRunningException rs)
    = showParen (d > appPrec) $
      showString "WorldNotRunningException" ∘ showsPrec (appPrec + 1) rs
    where
      appPrec = 10

instance Exception WorldNotRunningException

-- | An exception to be thrown when there was no player having the
-- given ID.
data UnknownPlayerIDException = UnknownPlayerIDException PlayerID
  deriving Show

instance Exception UnknownPlayerIDException
