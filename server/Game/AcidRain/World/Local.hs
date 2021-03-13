{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Local
  ( LocalWorld
  , newWorld
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, handle, toException)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (STM, atomically, throwSTM)
import qualified Data.UUID as U
import Game.AcidRain.World
  ( World(..), WorldMode(..), WorldState(..), WorldNotRunningException(..)
  , UnknownPlayerIDException(..) )
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Manager.Local (LocalChunkManager)
import qualified Game.AcidRain.World.Chunk.Manager.Local as LCM
import qualified Game.AcidRain.World.Chunk.Palette as Pal
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.Module (SomeModule)
import Game.AcidRain.Module.Loader (loadModules, lcTiles)
import Game.AcidRain.World.Player.Manager.Local (LocalPlayerManager)
import Game.AcidRain.World.Player (Player(..), Permission(..), PlayerID)
import Game.AcidRain.World.Position (WorldPos(..))
import qualified Game.AcidRain.World.Player.Manager.Local as LPM
import Prelude hiding (lcm)
import Prelude.Unicode ((∘), (⊥))


-- | Local world is a server-side world which is owned by a
-- server. The server accesses the world data directly. It is used
-- both in single player mode and multi player mode.
data LocalWorld
  = LocalWorld
    { lwMode   ∷ !WorldMode
    , lwState  ∷ !(TVar (WorldState RunningState))
    , lwThread ∷ ThreadId
    }

-- The state of running world. Not exposed to anywhere.
data RunningState
  = RunningState
    { rsChunks  ∷ !LocalChunkManager
    , rsPlayers ∷ !LocalPlayerManager
    } deriving Show

-- Assume the world is running. Not exposed to anywhere.
assumeRunning ∷ LocalWorld → STM RunningState
assumeRunning lw
  = do ws ← readTVar $ lwState lw
       case ws of
         Running rs → return rs
         _          → throwSTM $ WorldNotRunningException ws

instance World LocalWorld where
  type RunningStateT LocalWorld = RunningState

  getWorldState ∷ MonadIO μ ⇒ LocalWorld → μ (WorldState RunningState)
  getWorldState
    = liftIO ∘ atomically ∘ readTVar ∘ lwState

  lookupChunk ∷ MonadIO μ ⇒ ChunkPos → LocalWorld → μ (Maybe Chunk)
  lookupChunk pos lw
    = liftIO $ atomically $
      do rs ← assumeRunning lw
         LCM.lookup pos $ rsChunks rs

  -- FIXME: Remove this later.
  ensureChunkExists pos lw
    = liftIO $ atomically $
      do rs ← assumeRunning lw
         LCM.ensureChunkExists pos $ rsChunks rs

  getPlayer ∷ (MonadIO μ, MonadThrow μ) ⇒ PlayerID → LocalWorld → μ Player
  getPlayer pid lw
    = liftIO $ atomically $ assumeRunning lw >>= get'
    where
      get' rs | U.null pid = case lwMode lw of
                               SinglePlayer →
                                 -- The Nil player must have been
                                 -- spawned.
                                 LPM.get pid $ rsPlayers rs
                               MultiPlayer →
                                 -- Nil player can't exist in multi
                                 -- player mode.
                                 throwSTM $ UnknownPlayerIDException pid

              | otherwise  = case lwMode lw of
                               SinglePlayer →
                                 -- Only the Nil player can exist in
                                 -- single player mode.
                                 throwSTM $ UnknownPlayerIDException pid
                               MultiPlayer →
                                 -- Throw if the player doesn't exist.
                                 LPM.get pid $ rsPlayers rs

-- | Create a new world out of thin air. The world will be
-- asynchronously created on a separate thread.
newWorld ∷ (MonadIO μ, Foldable f) ⇒ WorldMode → f SomeModule → μ LocalWorld
newWorld wm mods
  = liftIO $
    do ws ← newTVarIO Loading
       let lw = LocalWorld
                { lwMode   = wm
                , lwState  = ws
                , lwThread = (⊥)
                }
       tid ← forkIO $ newWorld' lw >> runWorld lw
       return $ lw { lwThread = tid }
  where
    newWorld' ∷ LocalWorld → IO ()
    newWorld' lw
      = catchWhileLoading lw $
        atomically $
        do -- The first thing we need to do is to load modules. This
           -- can of course fail.
           lc  ← loadModules mods
           -- From now on we enter into an STM transaction. In order
           -- to construct the LCM, we need a tile palette.
           let tReg = lcTiles lc
               tPal = Pal.fromRegistry tReg
           lcm ← LCM.new tReg tPal
           -- And then create an empty LPM.
           lpm ← LPM.new
           -- If we are in single player mode, create the Nil player
           -- before anyone tries to join.
           let rs = RunningState
                    { rsChunks  = lcm
                    , rsPlayers = lpm
                    }
           void $ newPlayer U.nil Administrator rs

catchWhileLoading ∷ LocalWorld → IO () → IO ()
catchWhileLoading lw a
  = handle f a
  where
    f ∷ SomeException → IO ()
    f = atomically ∘ writeTVar (lwState lw) ∘ LoadFailed ∘ toException

runWorld ∷ LocalWorld → IO ()
runWorld _lw
  = error "FIXME: runWorld"

-- | Get the coordinate of the initial spawn.
initialSpawn ∷ RunningState → STM WorldPos
initialSpawn _rs
  -- FIXME: The initial spawn point can only be determined after
  -- generating the spawn chunk.
  = return $ WorldPos 0 0 0

-- | Spawn a new player in the world.
newPlayer ∷ PlayerID → Permission → RunningState → STM Player
newPlayer pid perm rs
  = do spawn ← initialSpawn rs
       let pl = Player
                { plID   = pid
                , plPerm = perm
                , plPos  = spawn
                }
       LPM.put pl $ rsPlayers rs
       return pl
