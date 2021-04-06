{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
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
  ( TBQueue, newTBQueueIO, tryReadTBQueue, readTBQueue
  , flushTBQueue, writeTBQueue )
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Eff (Eff, Member, Lift, Lifted, runLift, lift)
import Control.Eff.Exception (Exc, runError)
import Control.Eff.State.Strict (State, execState, runState, get, put)
import Control.Exception (Exception(..), SomeException, assert, handle)
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (STM, atomically, throwSTM, orElse)
import Data.Convertible.Base (convert)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.UUID as U
import GHC.Clock (getMonotonicTime)
import GHC.Conc (unsafeIOToSTM)
import Game.AcidRain.Module (SomeModule)
import Game.AcidRain.Module.Loader
  ( loadModules, lcTileReg, lcBiomeReg, lcEntityReg, lcCommandReg, lcChunkGen )
import qualified Game.AcidRain.Module.Builtin.Entities as B
import Game.AcidRain.World
  ( World(..), WorldMode(..), WorldState(..)
  , WorldSeed, WorldStateChanged(..), CommandSetUpdated(..), ChunkArrived(..)
  , ChunkUpdated(..)
  , WorldNotRunningException(..), UnknownPlayerIDException(..) )
import qualified Game.AcidRain.World.Biome.Palette as BPal
import Game.AcidRain.World.Chunk
  ( Chunk, putEntity, deleteEntity, entityAt, canEntityEnter )
import Game.AcidRain.World.Chunk.Manager.Local (LocalChunkManager)
import qualified Game.AcidRain.World.Chunk.Manager.Local as LCM
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Command
  ( Command(..), SomeCommand, IWorldCtx(..), WorldCtx
  , throwSomeExc )
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
import Lens.Micro ((^.), (&), (.~), traverseOf, traverseOf_)
import Lens.Micro.TH (makeLenses, makeLensesFor)
import Numeric.Natural (Natural)
import Prelude hiding (lcm)
import Prelude.Unicode ((∘), (⋅), (≡))


-- | Local world is a server-side world which is owned by a
-- server. The server accesses the world data directly. It is used
-- both in single player mode and multi player mode.
data LocalWorld
  = LocalWorld
    { _lwMode      ∷ !WorldMode
    , lwSeed       ∷ !WorldSeed
    , _lwState     ∷ !(TVar (WorldState RunningState))
    , _lwEvents    ∷ !(TBQueue SomeEvent)
    , _lwChunkReqs ∷ !(TBQueue ChunkPos)
    , _lwCommandQ  ∷ !(TBQueue QueuedCommand)
    }

data QueuedCommand
  = QueuedCommand
    { _qcCmd  ∷ !SomeCommand
    , _qcMPid ∷ !(Maybe PlayerID)
    , _qcArgs ∷ ![Text]
    } deriving Show

-- The state of running world. Not exposed to anywhere.
data RunningState
  = RunningState
    { _rsChunks   ∷ !LocalChunkManager
    , _rsPlayers  ∷ !LocalPlayerManager
    , _rsCommands ∷ !CommandRegistry
    } deriving Show

data LocalWorldCtx
  = LocalWorldCtx
    { _ctxLw            ∷ !LocalWorld
    , _ctxRs            ∷ !RunningState
      -- | Commands to run in the current tick.
    , _ctxCommandsToRun ∷ ![QueuedCommand]
      -- | Snapshots of all the chunks that can possibly be updated
      -- while ticking.
    , _ctxSnapshots     ∷ !(HashMap ChunkPos Chunk)
    }

data TickingState
  = -- | We have completed the last tick and will need to wait before
    -- starting the next tick.
    Waiting
    { _tsNextTick ∷ !Delay
    }
    -- | We are at a beginning of a tick.
  | BeginningOfTick
    { _tsTickBeganAt ∷ !Double
    }
    -- | We are in the middle of a tick.
  | MiddleOfTick
    { _tsTickBeganAt   ∷ !Double
    , _tsLocalWorldCtx ∷ !LocalWorldCtx
    }

makeLenses ''LocalWorld
makeLenses ''QueuedCommand
makeLenses ''RunningState
makeLenses ''LocalWorldCtx
makeLensesFor [("_tsLocalWorldCtx", "tsLocalWorldCtx")] ''TickingState

instance IWorldCtx LocalWorldCtx where
  basicFireEvent ctx
    = lift ∘ fireEvent (ctx^.ctxLw)

  basicGetPlayer ctx pid
    = do mp ← lift $ LPM.lookup pid (ctx^.ctxRs.rsPlayers)
         case mp of
           Just p  → return p
           Nothing → throwSomeExc $ UnknownPlayerIDException pid

  basicModifyPlayer ctx f pid
    = lift $ LPM.modify f pid (ctx^.ctxRs.rsPlayers)

  basicTryMoveEntity ctx0 src dest
    = do (succeeded, ctx1) ← runState ctx0 go
         if succeeded
           then return $ Just ctx1
           else return Nothing
    where
      go = do let lcm     = ctx0^.ctxRs.rsChunks
                  cpSrc   = convert src
                  cpDest  = convert dest
                  offSrc  = convert src
                  offDest = convert dest
              cSrc ← lift $ LCM.get cpSrc lcm
              case entityAt offSrc cSrc of
                Nothing  → return False -- But there's no entity here.
                Just ent →
                  -- The entity does exist. We still don't know if the
                  -- entity really can enter the destination.
                  if cpSrc ≡ cpDest
                  then
                    -- Both the source and the destination are in the
                    -- same chunk. So we only need to update one
                    -- chunk.
                    do let cp = cpSrc -- or cpDest. It doesn't matter.
                       c        ← lift $ LCM.get cp lcm
                       canEnter ← canEntityEnter offDest c
                       if canEnter
                         then do let c' = putEntity offDest ent $ deleteEntity offSrc cSrc
                                 snapshotChunk cp
                                 lift $ LCM.put cp c' lcm
                                 withWorldCtxUpcasted $
                                   E.entityMoved ent src dest
                                 return True
                         else return False
                  else
                    -- We are moving across a chunk boundary.
                    do cDest    ← lift $ LCM.get cpDest lcm
                       canEnter ← canEntityEnter offDest cDest
                       if canEnter
                         then do snapshotChunk cpSrc
                                 snapshotChunk cpDest
                                 lift $ LCM.modify (deleteEntity offSrc)   cpSrc  lcm
                                 lift $ LCM.modify (putEntity offDest ent) cpDest lcm
                                 withWorldCtxUpcasted $
                                   E.entityMoved ent src dest
                                 return $ True
                         else return False

eventQueueCapacity ∷ Natural
eventQueueCapacity = 256

-- Ticking interval in seconds.
tickingInterval ∷ Double
tickingInterval = 1/20

-- Assume the world is running or throw an exception. Not exposed to
-- anywhere.
assumeRunning ∷ LocalWorld → STM RunningState
assumeRunning lw
  = do ws ← readTVar (lw^.lwState)
       case ws of
         Running rs → return rs
         _          → throwSTM $ WorldNotRunningException ws

-- See if the world is running.
isRunning ∷ LocalWorld → STM (Maybe RunningState)
isRunning lw
  = do ws ← readTVar (lw^.lwState)
       case ws of
         Running rs → return (Just rs)
         _          → return Nothing

-- Fire a world event.
fireEvent ∷ Event e ⇒ LocalWorld → e → STM ()
fireEvent lw e
  = writeTBQueue (lw^.lwEvents) (upcastEvent e)

-- Change the world state.
changeState ∷ LocalWorld → WorldState RunningState → STM ()
changeState lw ws
  = do writeTVar (lw^.lwState) ws
       fireEvent lw $ WorldStateChanged ws

instance World LocalWorld where
  type RunningStateT LocalWorld = RunningState

  getWorldState
    = liftIO ∘ atomically ∘ readTVar ∘ (^.lwState)

  waitForEvent lw
    = liftIO $ atomically $
      do ev' ← tryReadTBQueue (lw^.lwEvents)
         case ev' of
           Just ev → return (Just ev)
           Nothing →
             do ws ← readTVar (lw^.lwState)
                case ws of
                  LoadFailed _ → return Nothing
                  Closed     _ → return Nothing
                  _            → Just <$> readTBQueue (lw^.lwEvents)

  scheduleCommand lw cmd mPid args
    = liftIO $ atomically $
      writeTBQueue (lw^.lwCommandQ) $
      QueuedCommand
      { _qcCmd  = upcastCommand cmd
      , _qcMPid = mPid
      , _qcArgs = args
      }

  lookupChunk lw pos
    = liftIO $ atomically $
      do rs ← assumeRunning lw
         cs ← LCM.lookup pos (rs^.rsChunks)
         case cs of
           Just  LCM.Loading   → return Nothing
           Just (LCM.Loaded c) → return $! Just c
           Nothing →
             -- Put a chunk retrieval request on a queue.
             do writeTBQueue (lw^.lwChunkReqs) pos
                return Nothing

  subscribeToChunks lw pid chunks
    = liftIO $ atomically $
      do rs ← assumeRunning lw
         LPM.subscribeToChunks pid chunks (rs^.rsPlayers)

  getPlayer lw pid
    = liftIO $ atomically $ assumeRunning lw >>= get'
    where
      get' rs | U.null pid = case lw^.lwMode of
                               SinglePlayer →
                                 -- The Nil player must have been
                                 -- spawned.
                                 LPM.get pid (rs^.rsPlayers)
                               MultiPlayer →
                                 -- Nil player can't exist in multi
                                 -- player mode.
                                 throwSTM $ UnknownPlayerIDException pid

              | otherwise  = case lw^.lwMode of
                               SinglePlayer →
                                 -- Only the Nil player can exist in
                                 -- single player mode.
                                 throwSTM $ UnknownPlayerIDException pid
                               MultiPlayer →
                                 -- Throw if the player doesn't exist.
                                 LPM.get pid (rs^.rsPlayers)

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
                { _lwMode      = wm
                , lwSeed       = seed
                , _lwState     = ws
                , _lwEvents    = es
                , _lwChunkReqs = cReqs
                , _lwCommandQ  = cCmdQ
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
                     { _rsChunks   = lcm
                     , _rsPlayers  = lpm
                     , _rsCommands = lc^.lcCommandReg
                     }
           atomically $
             do case wm of
                  SinglePlayer → void $ newPlayer U.nil Administrator rs
                  _            → return ()
                -- Notify clients of all the available commands.
                fireEvent lw $ CommandSetUpdated $ CR.valuesSet (rs^.rsCommands)
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
  = do ts ← BeginningOfTick <$> getMonotonicTime
       catchWhileRunning lw (loop ts)
  where
    loop ∷ TickingState → IO ()
    loop ts0
      = join $ atomically $
        do mrs ← isRunning lw
           case mrs of
             Nothing → return (return ())
             Just rs →
               ( do handleChunkReq lw rs
                    return $ loop ts0
               )
               `orElse`
               ( do ts1 ← waitForTickStart ts0 rs
                    ts2 ← traverseOf tsLocalWorldCtx consumeCommandQ ts1
                    if hasCommandsToRun ts2
                      then return $ loop ts2
                      else do tickWorld
                              traverseOf_ tsLocalWorldCtx notifyChunkUpdates ts2
                              return $ advanceTick ts2 >>= loop
               )

    tickWorld ∷ STM ()
    tickWorld = return () -- FIXME

    waitForTickStart ∷ TickingState → RunningState → STM TickingState
    waitForTickStart (Waiting delay) rs
      = do waitDelay delay
           now  ← unsafeIOToSTM $! getMonotonicTime
           cmds ← flushTBQueue (lw^.lwCommandQ)
           let ctx = LocalWorldCtx lw rs cmds HM.empty
           return $! MiddleOfTick now ctx

    waitForTickStart (BeginningOfTick beganAt) rs
      = do cmds ← flushTBQueue (lw^.lwCommandQ)
           let ctx = LocalWorldCtx lw rs cmds HM.empty
           return $ MiddleOfTick beganAt ctx

    waitForTickStart ts@(MiddleOfTick _ _) _
      = return ts

    advanceTick ∷ TickingState → IO TickingState
    advanceTick (Waiting _)
      = fail "Logical error: we are not in the middle of ticking"
    advanceTick (BeginningOfTick _)
      = fail "Logical error: we are not in the middle of ticking"
    advanceTick (MiddleOfTick startedAt ctx)
      = assert (null (ctx^.ctxCommandsToRun)) $
        do now ← getMonotonicTime
           -- The next tick time is basically now + tickingInterval,
           -- but if the world is lagging the interval becomes shorter
           -- than that, and can even go negative (no delay).
           let !elapsed = now - startedAt
               !delay   = tickingInterval - elapsed
           nextDelay ← newDelay (round (delay⋅1000000))
           return $! Waiting nextDelay

-- | Respond to chunk retrieval requests. This is run as a part of
-- 'runWorld'.
handleChunkReq ∷ LocalWorld → RunningState → STM ()
handleChunkReq lw rs
  = do cReq ← readTBQueue (lw^.lwChunkReqs)
       -- If the requested chunk is indeed not available, LCM.get
       -- spawns a thread and then retries the transaction. Thus we
       -- can respond to world termination immediately while
       -- generating the requested chunk.
       c ← LCM.get cReq (rs^.rsChunks)
       fireEvent lw $ ChunkArrived cReq c

-- | Check if a ticking state has commands to run.
hasCommandsToRun ∷ TickingState → Bool
hasCommandsToRun (MiddleOfTick _ ctx) = not $ null (ctx^.ctxCommandsToRun)
hasCommandsToRun _                    = False

-- | Run a single scheduled command in the world context. Retries when
-- nothing is scheduled. This is run as a part of 'runWorld'.
consumeCommandQ ∷ LocalWorldCtx → STM LocalWorldCtx
consumeCommandQ ctx
  = case ctx^.ctxCommandsToRun of
      []     → return ctx
      (c:cs) → do ctx' ← withWorldCtx ctx $
                         runOnWorld (c^.qcCmd) (c^.qcMPid) (c^.qcArgs)
                  return (ctx' & ctxCommandsToRun .~ cs)

withWorldCtx ∷ LocalWorldCtx
             → Eff '[State WorldCtx, Exc SomeException, Lift STM] ()
             → STM LocalWorldCtx
withWorldCtx ctx m
  = do eRes ← runLift $ runError $ execState (upcastWorldCtx ctx) m
       ctx' ←  handleCmdExc eRes
       return $ fromJust $ downcastWorldCtx ctx'
  where
    -- FIXME: Report it to the client without crashing the world.
    handleCmdExc ∷ Either SomeException a → STM a
    handleCmdExc (Left  e) = error ("FIXME: command failed: " ++ show e)
    handleCmdExc (Right a) = return a

withWorldCtxUpcasted ∷ Member (State LocalWorldCtx) r
                     ⇒ Eff (State WorldCtx : r) a
                     → Eff r a
withWorldCtxUpcasted m
  = do ctx0      ← get
       (a, ctx1) ← runState (upcastWorldCtx (ctx0 ∷ LocalWorldCtx)) m
       put (fromJust $ downcastWorldCtx ctx1 ∷ LocalWorldCtx)
       return a

-- Take a snapshot of a chunk before it can possibly be updated during
-- a tick.
snapshotChunk ∷ (Lifted STM r, Member (State LocalWorldCtx) r)
              ⇒ ChunkPos
              → Eff r ()
snapshotChunk cp
  = do ctx ← get
       ss' ← HM.alterF (f ctx) cp (ctx^.ctxSnapshots)
       put $ ctx & ctxSnapshots .~ ss'
  where
    f _   s@(Just _) = return s -- A snapshot has already been taken.
    f ctx Nothing    = do c ← lift $ LCM.get cp (ctx^.ctxRs.rsChunks)
                          return $ Just c

-- While ticking we take a snapshot of each chunk that can possibly be
-- updated. And now we are at the end of a tick. For each chunk we
-- compare it with the current state and if it's modified we notify
-- clients that are subscribing to the chunk.
--
-- But for now we assume all the chunks that have a snapshot have been
-- modified, because we aren't sure if we really want to make them
-- derive 'Eq'.
notifyChunkUpdates ∷ LocalWorldCtx → STM ()
notifyChunkUpdates ctx
  -- Pretty sure this isn't how foldrWithKey' is intended to be used,
  -- but HashMap just doesn't have a monadic variant of it.
  = HM.foldrWithKey' go (return ()) (ctx^.ctxSnapshots)
  where
    -- Is anyone subscribing to this chunk? If so fire 'ChunkUpdated'.
    go ∷ ChunkPos → Chunk → STM () → STM ()
    go cPos _ m
      = m *>
        do pls ← LPM.getSubscribers cPos (ctx^.ctxRs.rsPlayers)
           if not (HS.null pls)
             then fireEvent (ctx^.ctxLw) $ ChunkUpdated pls cPos
             else return ()

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
  = LCM.modify ins (convert pos) (rs^.rsChunks)
  where
    ins ∷ Chunk → Chunk
    ins = putEntity (convert pos) e

-- | Spawn a new player in the world.
newPlayer ∷ PlayerID → Permission → RunningState → STM Player
newPlayer pid perm rs
  = do spawn ← initialSpawn rs
       let pl = Player
                { _plID       = pid
                , _plPerm     = perm
                , _plPos      = spawn
                , _plIsOnline = True
                }
       LPM.put pl (rs^.rsPlayers)
       void $ spawnEntity spawn (B.Player pid) rs
       return pl
