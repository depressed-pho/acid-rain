{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World
  ( -- * The world class
    World(..)
  , WorldMode(..)
  , WorldState(..)
  , WorldSeed
  , SomeWorld(..)

    -- * Events
  , WorldStateChanged(..)

    -- * Exceptions
  , WorldNotRunningException(..)
  , UnknownPlayerIDException(..)
  ) where

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Event (Event(..), SomeEvent)
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
class World w where
  -- | The type of the running state of this world. It's usually an
  -- opaque type.
  type RunningStateT w ∷ Type
  -- | Erase the type of the world.
  upcastWorld ∷ w → SomeWorld
  upcastWorld = SomeWorld
  -- | Get the state of the world. State changes are also reported via
  -- 'WorldStateChanged' events. Unless explicitly stated, most of the
  -- other methods of this class throws exceptions when invoked at a
  -- wrong state.
  getWorldState ∷ MonadIO μ ⇒ w → μ (WorldState (RunningStateT w))
  -- | Block until the next world event is fired, or return 'Nothing'
  -- if these is no chance that any more events can ever fire.
  waitForEvent ∷ MonadIO μ ⇒ w → μ (Maybe SomeEvent)
  -- | Lookup a chunk at a certain position if it's available. This
  -- does not block. If the chunk isn't available yet, an event
  -- ChunkArrived will fire later.
  lookupChunk ∷ MonadIO μ ⇒ w → ChunkPos → μ (Maybe Chunk)
  -- FIXME: Remove this later.
  ensureChunkExists ∷ MonadIO μ ⇒ w → ChunkPos → μ ()
  -- | Get a player in the world having a given ID.
  getPlayer ∷ (MonadIO μ, MonadThrow μ) ⇒ w → PlayerID → μ Player

-- | A type-erased 'World'.
data SomeWorld = ∀w. World w ⇒ SomeWorld !w

instance World SomeWorld where
  type RunningStateT SomeWorld = ()
  upcastWorld = id
  getWorldState (SomeWorld w) = eraseRunningState <$> getWorldState w
    where
      eraseRunningState ∷ WorldState rs → WorldState ()
      eraseRunningState Loading        = Loading
      eraseRunningState LoadPending    = LoadPending
      eraseRunningState (LoadFailed e) = LoadFailed e
      eraseRunningState (Running _)    = Running ()
      eraseRunningState (Closed e)     = Closed e
  waitForEvent (SomeWorld w) = waitForEvent w
  lookupChunk (SomeWorld w) = lookupChunk w
  ensureChunkExists (SomeWorld w) = ensureChunkExists w
  getPlayer (SomeWorld w) = getPlayer w

data WorldMode
  = SinglePlayer
  | MultiPlayer
  deriving (Show, Eq)

data WorldState rs
    -- | The world is being loaded.
  = Loading
    -- | Concerns have been raised while loading the world. Players
    -- may choose to abort or continue loading the world, but until
    -- that the world waits for their input.
  | LoadPending
    -- | Problems have arised while loading the world.
  | LoadFailed !SomeException
    -- | The world is running.
  | Running !rs
    -- | The world has been closed.
  | Closed !(Maybe SomeException)
  deriving Show

-- | World seed is a 64-bit signed integer. Chunks generate randomly
-- but deterministically depending on the seed.
type WorldSeed = Int64

-- | An event to be fired when the 'WorldState' changes.
data WorldStateChanged
  = ∀rs. (Show rs, Typeable rs) ⇒ WorldStateChanged !(WorldState rs)

instance Show WorldStateChanged where
  -- GHC can't derive Show for this, but why?
  showsPrec d (WorldStateChanged rs)
    = showParen (d > appPrec) $
      showString "WorldStateChanged " ∘ showsPrec (appPrec + 1) rs
    where
      appPrec = 10

instance Event WorldStateChanged

-- | An exception to be thrown when a certain operation assuming the
-- world is running is attempted, but it was actually not running.
data WorldNotRunningException
  = ∀rs. (Show rs, Typeable rs) ⇒ WorldNotRunningException !(WorldState rs)

instance Show WorldNotRunningException where
  -- GHC can't derive Show for this, but why?
  showsPrec d (WorldNotRunningException rs)
    = showParen (d > appPrec) $
      showString "WorldNotRunningException " ∘ showsPrec (appPrec + 1) rs
    where
      appPrec = 10

instance Exception WorldNotRunningException

-- | An exception to be thrown when there was no player having the
-- given ID.
data UnknownPlayerIDException = UnknownPlayerIDException PlayerID
  deriving Show

instance Exception UnknownPlayerIDException
