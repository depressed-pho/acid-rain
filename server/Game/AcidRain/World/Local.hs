{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Local
  ( LocalWorld
  , newWorld
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.Delay (Delay, newDelay, waitDelay)
import Control.Concurrent.STM.TBQueue
  ( TBQueue, newTBQueueIO, tryReadTBQueue, readTBQueue, writeTBQueue )
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Data.Convertible.Base (convert)
import Control.Exception (Exception(..), SomeException, handle)
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (STM, atomically, throwSTM, orElse)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.UUID as U
import GHC.Conc (unsafeIOToSTM)
import Game.AcidRain.Module (SomeModule)
import Game.AcidRain.Module.Loader
  ( loadModules, lcTileReg, lcBiomeReg, lcEntityReg, lcCommandReg, lcChunkGen )
import qualified Game.AcidRain.Module.Builtin.Entities as B
import Game.AcidRain.World
  ( World(..), WorldMode(..), WorldState(..), WorldSeed, WorldStateChanged(..)
  , CommandSetUpdated(..), ChunkArrived(..)
  , WorldNotRunningException(..), UnknownPlayerIDException(..) )
import qualified Game.AcidRain.World.Biome.Palette as BPal
import Game.AcidRain.World.Chunk (Chunk, putEntity)
import Game.AcidRain.World.Chunk.Manager.Local (LocalChunkManager)
import qualified Game.AcidRain.World.Chunk.Manager.Local as LCM
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Command (Command(..), SomeCommand)
import Game.AcidRain.World.Command.Registry (CommandRegistry)
import qualified Game.AcidRain.World.Command.Registry as CR
import qualified Game.AcidRain.World.Entity as E
import qualified Game.AcidRain.World.Entity.Catalogue as ECat
import Game.AcidRain.World.Event (Event(..), SomeEvent)
import Game.AcidRain.World.Player.Manager.Local (LocalPlayerManager)
import Game.AcidRain.World.Player (Player(..), Permission(..), PlayerID)
import Game.AcidRain.World.Position (WorldPos(..))
import qualified Game.AcidRain.World.Player.Manager.Local as LPM
import qualified Game.AcidRain.World.Tile.Palette as TPal
import Lens.Micro ((^.))
import Numeric.Natural (Natural)
import Prelude hiding (lcm)
import Prelude.Unicode ((∘), (⋅))
import System.Clock (Clock(Monotonic), TimeSpec(..), getTime)


-- | Local world is a server-side world which is owned by a
-- server. The server accesses the world data directly. It is used
-- both in single player mode and multi player mode.
data LocalWorld
  = LocalWorld
    { lwMode      ∷ !WorldMode
    , lwSeed      ∷ !WorldSeed
    , lwState     ∷ !(TVar (WorldState RunningState))
    , lwEvents    ∷ !(TBQueue SomeEvent)
    , lwChunkReqs ∷ !(TBQueue ChunkPos)
    , lwCommandQ  ∷ !(TBQueue (SomeCommand, [Text]))
    }

eventQueueCapacity ∷ Natural
eventQueueCapacity = 256

-- | Ticking interval in μs.
tickingInterval ∷ Int
tickingInterval = 100 ⋅ 1000

-- The state of running world. Not exposed to anywhere.
data RunningState
  = RunningState
    { rsChunks   ∷ !LocalChunkManager
    , rsPlayers  ∷ !LocalPlayerManager
    , rsCommands ∷ !CommandRegistry
    } deriving Show

-- Assume the world is running or throw an exception. Not exposed to
-- anywhere.
assumeRunning ∷ LocalWorld → STM RunningState
assumeRunning lw
  = do ws ← readTVar (lwState lw)
       case ws of
         Running rs → return rs
         _          → throwSTM $ WorldNotRunningException ws

-- See if the world is running.
isRunning ∷ LocalWorld → STM (Maybe RunningState)
isRunning lw
  = do ws ← readTVar (lwState lw)
       case ws of
         Running rs → return (Just rs)
         _          → return Nothing

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
    = liftIO ∘ atomically ∘ readTVar ∘ lwState

  waitForEvent lw
    = liftIO $ atomically $
      do ev' ← tryReadTBQueue (lwEvents lw)
         case ev' of
           Just ev → return (Just ev)
           Nothing →
             do ws ← readTVar (lwState lw)
                case ws of
                  LoadFailed _ → return Nothing
                  Closed     _ → return Nothing
                  _            → Just <$> readTBQueue (lwEvents lw)

  scheduleCommand lw cmd args
    = liftIO $ atomically $
      writeTBQueue (lwCommandQ lw) (upcastCommand cmd, args)

  lookupChunk lw pos
    = liftIO $ atomically $
      do rs ← assumeRunning lw
         mc ← LCM.lookup pos $ rsChunks rs
         if isJust mc
           then return mc
           else do -- Put a chunk retrieval request on a queue.
                   writeTBQueue (lwChunkReqs lw) pos
                   return mc

  getPlayer lw pid
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
newWorld ∷ (Foldable f, MonadIO μ) ⇒ WorldMode → f SomeModule → WorldSeed → μ LocalWorld
newWorld wm mods seed
  = liftIO $
    do ws    ← newTVarIO Loading
       es    ← newTBQueueIO eventQueueCapacity
       cReqs ← newTBQueueIO eventQueueCapacity
       cCmdQ ← newTBQueueIO eventQueueCapacity
       let lw = LocalWorld
                { lwMode      = wm
                , lwSeed      = seed
                , lwState     = ws
                , lwEvents    = es
                , lwChunkReqs = cReqs
                , lwCommandQ  = cCmdQ
                }
       void $ forkIO $ newWorld' lw >> runWorld lw
       return lw
  where
    newWorld' ∷ LocalWorld → IO ()
    newWorld' lw
      = catchWhileLoading lw $
        do rs ← atomically $
                do -- The first thing we need to do is to load
                   -- modules. This can of course fail.
                   lc  ← loadModules mods seed
                   -- From now on we enter into an STM transaction. In
                   -- order to construct the LCM, we need a tile
                   -- palette. Constructing a tile palette never fails
                   -- because we are doing it from scratch.
                   let tReg = lc^.lcTileReg
                       tPal = TPal.fromRegistry tReg
                       -- And we also need a biome palette.
                       bReg = lc^.lcBiomeReg
                       bPal = BPal.fromRegistry bReg
                       -- And an entity catalogue.
                       eReg = lc^.lcEntityReg
                       eCat = ECat.fromRegistry eReg
                       -- And a chunk generator too.
                       cGen = lc^.lcChunkGen
                   lcm ← LCM.new tReg tPal bReg bPal eCat cGen
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
                     { rsChunks   = lcm
                     , rsPlayers  = lpm
                     , rsCommands = lc^.lcCommandReg
                     }
           atomically $
             do case wm of
                  SinglePlayer → void $ newPlayer U.nil Administrator rs
                  _            → return ()
                -- Notify clients of all the available commands.
                fireEvent lw $ CommandSetUpdated $ CR.valuesSet (rsCommands rs)
                -- Okay, now we can run the world but before that we
                -- change the state.
                changeState lw $ Running rs

catchWhileLoading ∷ LocalWorld → IO () → IO ()
catchWhileLoading lw a
  = handle f a
  where
    f ∷ SomeException → IO ()
    f = atomically ∘ changeState lw ∘ LoadFailed ∘ toException

catchWhileRunning ∷ LocalWorld → IO () → IO ()
catchWhileRunning lw a
  = handle f a
  where
    f ∷ SomeException → IO ()
    f = atomically ∘ changeState lw ∘ Closed ∘ Just ∘ toException

runWorld ∷ LocalWorld → IO ()
runWorld lw
  = do nextTick ← getNextTick 0
       catchWhileRunning lw (loop nextTick)
  where
    loop ∷ Delay → IO ()
    loop nextTick
      = join $ atomically $
        do mrs ← isRunning lw
           case mrs of
             Nothing → return (return ())
             Just rs →
               do handleChunkReq rs
                  return $ loop nextTick
               `orElse`
               do lastTick ← waitForNextTick nextTick
                  (consumeCommandQ `orElse` tickWorld)
                  return $ getNextTick lastTick >>= loop

    handleChunkReq ∷ RunningState → STM ()
    handleChunkReq rs
      = do cReq ← readTBQueue (lwChunkReqs lw)
           -- If the requested chunk is indeed not available, LCM.get
           -- spawns a thread and then retries the transaction. Thus
           -- we can respond to world termination immediately while
           -- generating the requested chunk.
           c ← LCM.get cReq (rsChunks rs)
           fireEvent lw $ ChunkArrived cReq c

    consumeCommandQ ∷ STM ()
    consumeCommandQ = return () -- FIXME

    tickWorld ∷ STM ()
    tickWorld = return () -- FIXME

    getNextTick ∷ TimeSpec → IO Delay
    getNextTick lastTick
      = do now ← getTime Monotonic
           -- The next tick time is basically now + tickingInterval,
           -- but if the world is lagging the interval becomes shorter
           -- than that, and can even go negative (no delay).
           let !elapsed = μs (now - lastTick)
               !delay   = tickingInterval - elapsed
           newDelay delay

    μs ∷ TimeSpec → Int
    μs !(TimeSpec {sec, nsec})
      = fromIntegral $! (sec ⋅ 1000000) + (nsec `div` 1000)

    waitForNextTick ∷ Delay → STM TimeSpec
    waitForNextTick nextTick
      = do waitDelay nextTick
           unsafeIOToSTM $! getTime Monotonic

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
