{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Local
  ( LocalWorld
  , newWorld
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TBQueue
  ( TBQueue, newTBQueueIO, tryReadTBQueue, readTBQueue, writeTBQueue )
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Eff (Eff, Lifted, lift, run, runLift)
import Control.Eff.Exception (Exc, runError)
import Data.Convertible.Base (convert)
import Control.Exception (Exception(..), SomeException, handle)
import Control.Monad (void)
import Control.Monad.STM (STM, atomically, throwSTM)
import qualified Data.UUID as U
import Game.AcidRain.Module (SomeModule)
import Game.AcidRain.Module.Loader
  ( loadModules, lcTiles, lcEntityTypes, lcChunkGen )
import qualified Game.AcidRain.Module.Builtin.Entity as B
import Game.AcidRain.World
  ( World(..), WorldMode(..), WorldState(..), WorldSeed, WorldStateChanged(..)
  , WorldNotRunningException(..), UnknownPlayerIDException(..) )
import Game.AcidRain.World.Chunk (Chunk, putEntity)
import Game.AcidRain.World.Chunk.Manager.Local (LocalChunkManager)
import qualified Game.AcidRain.World.Chunk.Manager.Local as LCM
import qualified Game.AcidRain.World.Chunk.Palette as Pal
import qualified Game.AcidRain.World.Entity as E
import qualified Game.AcidRain.World.Entity.Catalogue as ECat
import Game.AcidRain.World.Event (Event(..), SomeEvent)
import Game.AcidRain.World.Player.Manager.Local (LocalPlayerManager)
import Game.AcidRain.World.Player (Player(..), Permission(..), PlayerID)
import Game.AcidRain.World.Position (WorldPos(..))
import qualified Game.AcidRain.World.Player.Manager.Local as LPM
import Lens.Micro ((^.))
import Numeric.Natural (Natural)
import Prelude hiding (lcm)
import Prelude.Unicode ((∘))

-- | Local world is a server-side world which is owned by a
-- server. The server accesses the world data directly. It is used
-- both in single player mode and multi player mode.
data LocalWorld
  = LocalWorld
    { lwMode   ∷ !WorldMode
    , lwSeed   ∷ !WorldSeed
    , lwState  ∷ !(TVar (WorldState RunningState))
    , lwEvents ∷ !(TBQueue SomeEvent)
    }

eventQueueCapacity ∷ Natural
eventQueueCapacity = 256

-- The state of running world. Not exposed to anywhere.
data RunningState
  = RunningState
    { rsChunks  ∷ !LocalChunkManager
    , rsPlayers ∷ !LocalPlayerManager
    } deriving Show

-- Assume the world is running or throw an exception. Not exposed to
-- anywhere.
assumeRunning ∷ LocalWorld → STM RunningState
assumeRunning lw
  = do ws ← readTVar $ lwState lw
       case ws of
         Running rs → return rs
         _          → throwSTM $ WorldNotRunningException ws

-- Keep running a supplied IO action while the world is running.
repeatWhileRunning ∷ LocalWorld → (RunningState → IO ()) → IO ()
repeatWhileRunning lw f
  = do ws ← atomically $ readTVar $ lwState lw
       case ws of
         Running rs → f rs >> repeatWhileRunning lw f
         _          → return ()

-- Fire a world event.
fireEvent ∷ Event e ⇒ LocalWorld → e → STM ()
fireEvent lw e
  = writeTBQueue (lwEvents lw) (upcastEvent e)

-- Change the world state.
changeState ∷ LocalWorld → WorldState RunningState → STM ()
changeState lw ws
  = do writeTVar (lwState lw) ws
       fireEvent lw $ WorldStateChanged ws

instance World LocalWorld where
  type RunningStateT LocalWorld = RunningState

  getWorldState
    = lift ∘ atomically ∘ readTVar ∘ lwState

  waitForEvent lw
    = lift $ atomically $
      do ev' ← tryReadTBQueue (lwEvents lw)
         case ev' of
           Just ev → return (Just ev)
           Nothing →
             do ws ← readTVar (lwState lw)
                case ws of
                  LoadFailed _ → return Nothing
                  Closed     _ → return Nothing
                  _            → Just <$> readTBQueue (lwEvents lw)

  lookupChunk lw pos
    = lift $ atomically $
      do rs ← assumeRunning lw
         LCM.lookup pos $ rsChunks rs

  -- FIXME: Remove this later.
  ensureChunkExists lw pos
    = lift $ atomically $
      do rs ← assumeRunning lw
         void $ LCM.get pos $ rsChunks rs

  getPlayer lw pid
    = lift $ atomically $ assumeRunning lw >>= get'
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
newWorld ∷ (Lifted IO r, Foldable f) ⇒ WorldMode → f SomeModule → WorldSeed → Eff r LocalWorld
newWorld wm mods seed
  = lift $
    do ws ← newTVarIO Loading
       es ← newTBQueueIO eventQueueCapacity
       let lw = LocalWorld
                { lwMode   = wm
                , lwSeed   = seed
                , lwState  = ws
                , lwEvents = es
                }
       void $ forkIO $ runLift (newWorld' lw) >> runWorld lw
       return lw
  where
    newWorld' ∷ Lifted IO r ⇒ LocalWorld → Eff r ()
    newWorld' lw
      = catchWhileLoading lw $
        do rs ← lift $ atomically $
                do -- The first thing we need to do is to load
                   -- modules. This can of course fail.
                   lc  ← runErrorInSTM $ loadModules mods seed
                   -- From now on we enter into an STM transaction. In
                   -- order to construct the LCM, we need a tile
                   -- palette. Constructing a tile palette never fails
                   -- because we are doing it from scratch.
                   let tReg = lc^.lcTiles
                       tPal = Pal.fromRegistry tReg
                       -- And we also need an entity catalogue.
                       eReg = lc^.lcEntityTypes
                       eCat = ECat.fromRegistry eReg
                       -- And a chunk generator too.
                       cGen = lc^.lcChunkGen
                   lcm ← LCM.new tReg tPal eCat cGen
                   -- And then create an empty LPM.
                   lpm ← LPM.new
                   -- If we are in single player mode, create the Nil
                   -- player before anyone tries to join. This means
                   -- we need to generate a chunk for the player to
                   -- spawn, but it's going to retry the transaction
                   -- because there are no chunks initially. So commit
                   -- it and start a new one. It's okay because we are
                   -- still in the Loading state.
                   return $ RunningState
                     { rsChunks  = lcm
                     , rsPlayers = lpm
                     }
           lift $ atomically $
             do case wm of
                  SinglePlayer → void $ newPlayer U.nil Administrator rs
                  _            → return ()
                -- Okay, now we can run the world but before that we
                -- change the state.
                changeState lw $ Running rs

runErrorInSTM ∷ Exception e ⇒ Eff '[Exc e] α → STM α
runErrorInSTM m
  = case run (runError m) of
      Right a → return a
      Left  e → throwSTM e

catchWhileLoading ∷ Lifted IO r ⇒ LocalWorld → Eff (Exc SomeException : r) () → Eff r ()
catchWhileLoading lw m
  = do a ← runError m
       case a of
         Right () → return ()
         Left e   → lift $ atomically $ changeState lw $ LoadFailed e

catchWhileRunning ∷ LocalWorld → IO () → IO ()
catchWhileRunning lw a
  = handle f a
  where
    f ∷ SomeException → IO ()
    f = atomically ∘ changeState lw ∘ Closed ∘ Just ∘ toException

runWorld ∷ LocalWorld → IO ()
runWorld lw
  = repeatWhileRunning lw runWorld'
  where
    runWorld' ∷ RunningState → IO ()
    runWorld' _rs
      = catchWhileRunning lw $
        do threadDelay 10000000 -- FIXME

-- | Get the coordinate of the initial spawn.
initialSpawn ∷ RunningState → STM WorldPos
initialSpawn _rs
  -- FIXME: The initial spawn point can only be determined after
  -- generating the spawn chunk.
  = return $ WorldPos 0 0 0

-- | Spawn an entity somewhere in the world. Generate a chunk if it
-- doesn't exist yet.
spawnEntity ∷ E.Entity ε
            ⇒ WorldPos
            → ε
            → RunningState
            → STM ()
spawnEntity pos e rs
  = LCM.modify ins (convert pos) (rsChunks rs)
  where
    ins ∷ Chunk → Chunk
    ins = putEntity (convert pos) e

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
       void $ spawnEntity spawn (B.Player pid) rs
       return pl
