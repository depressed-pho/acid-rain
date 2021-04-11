{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World
  ( -- * The world class and accompanying types
    World(..)
  , WorldMode(..)
  , WorldState(..)
  , WorldSeed
  , SomeWorld(..)

    -- * The command class
  , Command(..)
  , CommandID
  , CommandType(..)
  , SomeCommand(..)

    -- * Client-side context
  , IClientCtx(..)
  , ClientCtx
  , getClientWorld
  , getClientPlayerID
  , sendToWorld
  , hasWindow
  , insertWindow
  , deleteWindow

    -- * World-side context
  , IWorldCtx(..)
  , WorldCtx
  , fireEvent
  , getPlayer
  , modifyPlayer
  , tryMoveEntity

    -- * Events
  , WorldStateChanged(..)
  , CommandSetUpdated(..)
  , ChunkArrived(..)
  , ChunkUpdated(..)

    -- * Exceptions
  , WorldNotRunningException(..)
  , UnknownPlayerIDException(..)
  , PlayerOfflineException(..)
  , BadArgumentsException(..)
  , ClientOnlyCommandException(..)
  , throwSomeExc
  , throwSomeExc_
  ) where

import Control.Eff (Eff, Lifted, Member, type(<::))
import Control.Eff.Exception (Exc, throwError, throwError_)
import Control.Eff.State.Strict (State, get, put)
import Control.Exception (Exception(..), SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (STM)
import Data.Default (Default(..))
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Data.Kind (Type)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Event (Event(..), SomeEvent)
import Game.AcidRain.World.Player (Player, PlayerID)
import Game.AcidRain.World.Position (WorldPos)
import Game.AcidRain.TUI.Keystroke (Keystroke)
import Game.AcidRain.TUI.Window (Window, WindowID)
import Prelude.Unicode ((∘), (≡))


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
  -- | Schedule a command (along with arguments) to run on the world
  -- context in the next game tick. The argument @mPid@ indicates
  -- the player who attempted to use the command, or 'Nothing' if
  -- there is no particular player who did it.
  scheduleCommand ∷ (MonadIO μ, Command c)
                  ⇒ w
                  → c
                  → Maybe PlayerID -- ^ @mPid@
                  → [Text]         -- ^ Arguments
                  → μ ()
  -- | Lookup a chunk at a certain position if it's available. This
  -- does not block. If the chunk isn't available yet, an event
  -- ChunkArrived will fire later.
  lookupChunk ∷ MonadIO μ ⇒ w → ChunkPos → μ (Maybe Chunk)
  -- | Subscribe to 'ChunkUpdated' events that will fire for a client
  -- controlling a given player. Previous subscription will be
  -- cancelled.
  subscribeToChunks ∷ MonadIO μ
                    ⇒ w
                    → PlayerID
                    → (ChunkPos, ChunkPos) -- ^ @(topLeft, bottomRight)@
                    → μ ()
  -- | Get a player in the world having a given ID.
  getPlayerFromWorld ∷ MonadIO μ ⇒ w → PlayerID → μ Player

-- | A type-erased 'World'.
data SomeWorld = ∀w. World w ⇒ SomeWorld !w

instance World SomeWorld where
  type RunningStateT SomeWorld = ()
  upcastWorld = id
  getWorldState (SomeWorld w) = (() <$) <$> getWorldState w
  waitForEvent (SomeWorld w) = waitForEvent w
  scheduleCommand (SomeWorld w) = scheduleCommand w
  lookupChunk (SomeWorld w) = lookupChunk w
  subscribeToChunks (SomeWorld w) = subscribeToChunks w
  getPlayerFromWorld (SomeWorld w) = getPlayerFromWorld w

data WorldMode
  = SinglePlayer
  | MultiPlayer
  deriving (Show, Eq)

data WorldState rs
    -- | The world is being loaded. This is the initial state.
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

instance Default (WorldState rs) where
  def = Loading

instance Functor WorldState where
  fmap _ Loading        = Loading
  fmap _ LoadPending    = LoadPending
  fmap _ (LoadFailed e) = LoadFailed e
  fmap f (Running rs)   = Running (f rs)
  fmap _ (Closed me)    = Closed me

-- | World seed is a 64-bit signed integer. Chunks generate randomly
-- but deterministically depending on the seed.
type WorldSeed = Int64

-------------------------------------------------------------------------------
-- The command class
-------------------------------------------------------------------------------
type CommandID = Text

data CommandType
  = -- | Regular commands can only be invoked on a command line.
    Regular
    -- | Interactive commands can be invoked with no arguments. A
    -- keystroke can therefore be assigned to them. The @'Maybe'
    -- 'Keystroke'@ is a keystroke that should be assigned by default.
  | Interactive !(Maybe Keystroke)
  deriving (Eq, Show, Generic)

class (Show c, Typeable c) ⇒ Command c where
  -- | Erase the type of the command.
  upcastCommand ∷ c → SomeCommand
  upcastCommand = SomeCommand
  -- | Recover the type of the command.
  downcastCommand ∷ SomeCommand → Maybe c
  downcastCommand (SomeCommand c) = cast c
  -- | Get the command ID such as @acid-rain:walk-south@.
  commandID ∷ c → CommandID
  -- | Get the command type.
  commandType ∷ c → CommandType
  -- | Run the command on the client-side context. The default
  -- implementation simply redirects the command to the world.
  runOnClient ∷ (MonadIO (Eff r), [State ClientCtx, Exc SomeException] <:: r)
              ⇒ c
              → [Text]
              → Eff r ()
  runOnClient = sendToWorld
  -- | Run the command on the world-side context.
  --
  -- The argument @mPid@ indicates the player who invoked the command,
  -- or 'Nothing' if there is no particular player who did it. Doing a
  -- security check is a responsibility of this method which is
  -- usually done via (FIXME: which functions can we use?)
  --
  -- Throwing dynamic exceptions in this method would result in a
  -- server crash. It should therefore be avoided unless something
  -- catastrophic happens. For regular errors use 'Exc'.
  runOnWorld ∷ (Lifted STM r, [State WorldCtx, Exc SomeException] <:: r)
             ⇒ c
             → Maybe PlayerID -- ^ @mPid@
             → [Text]         -- ^ Arguments
             → Eff r ()

-- | A type-erased 'Command'
data SomeCommand = ∀c. Command c ⇒ SomeCommand !c

instance Show SomeCommand where
  showsPrec d (SomeCommand c) = showsPrec d c

-- | Two commands are equivalent if their IDs are identical.
instance Eq SomeCommand where
  (SomeCommand a) == (SomeCommand b)
    = (commandID a) ≡ (commandID b)

-- | Hash value of a command is a hash value of its ID.
instance Hashable SomeCommand where
  hashWithSalt salt (SomeCommand c)
    = salt `hashWithSalt` commandID c

instance Command SomeCommand where
  upcastCommand = id
  downcastCommand = Just
  commandID (SomeCommand c) = commandID c
  commandType (SomeCommand c) = commandType c
  runOnClient (SomeCommand c) = runOnClient c
  runOnWorld (SomeCommand c) = runOnWorld c

-------------------------------------------------------------------------------
-- Client-side context
-------------------------------------------------------------------------------
-- | Command-evaluating context for the client side. Command
-- implementations should not invoke its methods directly.
class Typeable ctx ⇒ IClientCtx ctx where
  -- | Erase the type of 'IClientCtx'.
  upcastClientCtx ∷ ctx → ClientCtx
  upcastClientCtx = ClientCtx
  -- | Recover the type of 'IClientCtx'.
  downcastClientCtx ∷ ClientCtx → Maybe ctx
  downcastClientCtx (ClientCtx ctx) = cast ctx
  -- | Get the world the client controls.
  basicGetClientWorld ∷ ctx → SomeWorld
  -- | Get the ID of the player whom the client controls.
  basicGetClientPlayerID ∷ ctx → PlayerID
  -- | Return 'True' iff a window with the given ID is shown.
  basicHasWindow ∷ WindowID → ctx → Bool
  -- | Insert a new window to the client.
  basicInsertWindow ∷ (MonadIO μ, Window w) ⇒ w → ctx → μ ctx
  -- | Delete windows with the given ID from the client.
  basicDeleteWindow ∷ WindowID → ctx → ctx

-- | Type-erased 'IClientCtx'. We hate this. What we really want to do
-- is @('IClientCtx' ctx, 'Member' ('State' ctx) r) ⇒ 'Eff' r a@ and
-- not @'Member' ('State' 'ClientCtx') r ⇒ 'Eff' r a@, but polymorphic
-- effect types just don't play nice with open unions. So we work
-- around the problem by temporarily erasing the type and recovering
-- it afterwards.
data ClientCtx = ∀ctx. IClientCtx ctx ⇒ ClientCtx !ctx

instance IClientCtx ClientCtx where
  upcastClientCtx = id
  downcastClientCtx = Just
  basicGetClientWorld (ClientCtx ctx) = basicGetClientWorld ctx
  basicGetClientPlayerID (ClientCtx ctx) = basicGetClientPlayerID ctx
  basicHasWindow wid (ClientCtx ctx) = basicHasWindow wid ctx
  basicInsertWindow w (ClientCtx ctx) = ClientCtx <$> basicInsertWindow w ctx
  basicDeleteWindow wid (ClientCtx ctx) = ClientCtx $ basicDeleteWindow wid ctx

-- | Get the world the client controls.
getClientWorld ∷ Member (State ClientCtx) r ⇒ Eff r SomeWorld
getClientWorld
  = do ctx ← get
       return $ basicGetClientWorld (ctx ∷ ClientCtx)

-- | Get the ID of the player whom the client controls.
getClientPlayerID ∷ Member (State ClientCtx) r ⇒ Eff r PlayerID
getClientPlayerID
  = do ctx ← get
       return $ basicGetClientPlayerID (ctx ∷ ClientCtx)

-- | Send a command (along with arguments) from a client context to a
-- world, scheduling it to run on the world context.
sendToWorld ∷ (MonadIO (Eff r), Member (State ClientCtx) r, Command c)
            ⇒ c
            → [Text]
            → Eff r ()
sendToWorld c args
  = do w   ← getClientWorld
       pid ← getClientPlayerID
       liftIO $ scheduleCommand w c (Just pid) args

-- | Return 'True' iff a window with the given ID is shown.
hasWindow ∷ Member (State ClientCtx) r ⇒ WindowID → Eff r Bool
hasWindow wid
  = do ctx ← get
       return $ basicHasWindow wid (ctx ∷ ClientCtx)

-- | Insert a new window to the client.
insertWindow ∷ (Window w, MonadIO (Eff r), Member (State ClientCtx) r) ⇒ w → Eff r ()
insertWindow win
  = do ctx  ← get
       ctx' ← basicInsertWindow win (ctx ∷ ClientCtx)
       put ctx'

-- | Delete windows with the given ID from the client.
deleteWindow ∷ Member (State ClientCtx) r ⇒ WindowID → Eff r ()
deleteWindow wid
  = do ctx ← get
       put $ basicDeleteWindow wid (ctx ∷ ClientCtx)

-------------------------------------------------------------------------------
-- World-side context
-------------------------------------------------------------------------------
-- | Command-evaluating and ticking context for the world side
-- (aka. server-side).
class Typeable ctx ⇒ IWorldCtx ctx where
  -- | Erase the type of 'IWorldCtx'.
  upcastWorldCtx ∷ ctx → WorldCtx
  upcastWorldCtx = WorldCtx
  -- | Recover the type of 'IWorldCtx'.
  downcastWorldCtx ∷ WorldCtx → Maybe ctx
  downcastWorldCtx (WorldCtx ctx) = cast ctx
  -- | Fire a world event.
  basicFireEvent ∷ (Lifted STM r, Event e) ⇒ ctx → e → Eff r ()
  -- | Get a player in the world having a given ID.
  basicGetPlayer ∷ (Lifted STM r, Member (Exc SomeException) r)
                 ⇒ ctx
                 → PlayerID
                 → Eff r Player
  -- | Modify a player in the world having a given ID.
  basicModifyPlayer ∷ (Lifted STM r, Member (Exc SomeException) r)
                    ⇒ ctx
                    → (Player → Player)
                    → PlayerID
                    → Eff r ()
  -- | Move an entity in the world and return 'Just' iff successful.
  basicTryMoveEntity ∷ (Lifted STM r, Member (Exc SomeException) r)
                     ⇒ ctx
                     → WorldPos -- ^ source
                     → WorldPos -- ^ destination
                     → Eff r (Maybe ctx)

-- | Type-erased 'IWorldCtx'. We hate this for the same reason as
-- 'ClientCtx'.
data WorldCtx = ∀ctx. IWorldCtx ctx ⇒ WorldCtx !ctx

instance IWorldCtx WorldCtx where
  upcastWorldCtx = id
  downcastWorldCtx = Just
  basicFireEvent (WorldCtx ctx) = basicFireEvent ctx
  basicGetPlayer (WorldCtx ctx) = basicGetPlayer ctx
  basicModifyPlayer (WorldCtx ctx) = basicModifyPlayer ctx
  basicTryMoveEntity (WorldCtx ctx) src dest
    = (WorldCtx <$>) <$> basicTryMoveEntity ctx src dest

-- | Fire a world event.
fireEvent ∷ (Lifted STM r, Member (State WorldCtx) r, Event e) ⇒ e → Eff r ()
fireEvent e
  = do ctx ← get
       basicFireEvent (ctx ∷ WorldCtx) e

-- | Get a player in the world having a given ID.
getPlayer ∷ (Lifted STM r, [State WorldCtx, Exc SomeException] <:: r)
          ⇒ PlayerID
          → Eff r Player
getPlayer pid
  = do ctx ← get
       basicGetPlayer (ctx ∷ WorldCtx) pid

-- | Modify a player in the world having a given ID.
modifyPlayer ∷ (Lifted STM r, [State WorldCtx, Exc SomeException] <:: r)
             ⇒ (Player → Player)
             → PlayerID
             → Eff r ()
modifyPlayer f pid
  = do ctx ← get
       basicModifyPlayer (ctx ∷ WorldCtx) f pid

-- | Move an entity in the world and return 'True' iff successful.
tryMoveEntity ∷ (Lifted STM r, [State WorldCtx, Exc SomeException] <:: r)
              ⇒ WorldPos -- ^ source
              → WorldPos -- ^ destination
              → Eff r Bool
tryMoveEntity src dest
  = do ctx  ← get
       mCtx ← basicTryMoveEntity (ctx ∷ WorldCtx) src dest
       case mCtx of
         Just ctx' → put ctx' *> return True
         Nothing   → return False

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------
-- | Event to be fired when the 'WorldState' changes.
data WorldStateChanged
  = ∀rs. (Show rs, Typeable rs) ⇒ WorldStateChanged !(WorldState rs)
  deriving Event

instance Show WorldStateChanged where
  -- GHC can't derive Show for this, but why?
  showsPrec d (WorldStateChanged rs)
    = showParen (d > appPrec) $
      showString "WorldStateChanged " ∘ showsPrec (appPrec + 1) rs
    where
      appPrec = 10

-- | Event to be fired when the set available commands have been
-- updated. In particular, this event always fires right before a
-- world starts running.
newtype CommandSetUpdated = CommandSetUpdated (HashSet SomeCommand)
  deriving (Show, Event)

-- | Event to be fired when a chunk which was previously unavailable
-- is now available.
data ChunkArrived = ChunkArrived !ChunkPos !Chunk
  deriving Event

instance Show ChunkArrived where
  showsPrec d (ChunkArrived cPos _)
    = showParen (d > appPrec) $
      showString "ChunkArrived " ∘
      showsPrec (appPrec + 1) cPos ∘
      showString " .."
    where
      appPrec = 10

-- | Event to be fired when the contents of a chunk have changed. This
-- event is fired only when there is at least one player which is
-- subscribing to the chunk.
data ChunkUpdated
  = ChunkUpdated
    !(HashSet PlayerID) -- ^ Set of players who have subscribed to the chunk.
    !ChunkPos
    -- !ChunkDiff -- FIXME
  deriving (Show, Event)

-------------------------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------------------------
-- | Exception to be thrown when a certain operation assuming the
-- world is running is attempted, but it was actually not running.
data WorldNotRunningException
  = ∀rs. (Show rs, Typeable rs) ⇒ WorldNotRunningException !(WorldState rs)
  deriving Exception

instance Show WorldNotRunningException where
  -- GHC can't derive Show for this, but why?
  showsPrec d (WorldNotRunningException rs)
    = showParen (d > appPrec) $
      showString "WorldNotRunningException " ∘ showsPrec (appPrec + 1) rs
    where
      appPrec = 10

-- | Exception to be thrown when there was no player having the given
-- ID.
data UnknownPlayerIDException = UnknownPlayerIDException !PlayerID
  deriving (Show, Exception)

-- | Exception to be thrown when an operation expecting a player to be
-- online is attempted while the player is offline.
data PlayerOfflineException = PlayerOfflineException !PlayerID
  deriving (Show, Exception)

-- | Attempted to run a command with invalid arguments.
data BadArgumentsException = BadArgumentsException deriving
  (Show, Exception)

-- | Attempted to run a client-only command on the world-side.
data ClientOnlyCommandException = ClientOnlyCommandException
  deriving (Show, Exception)

-- | A utility function to throw any 'Exception' using 'Exc' effect.
throwSomeExc ∷ (Exception e, Member (Exc SomeException) r) ⇒ e → Eff r a
throwSomeExc = throwError ∘ toException

-- | A variant of 'throwSomeExc' whose return type is the unit.
throwSomeExc_ ∷ (Exception e, Member (Exc SomeException) r) ⇒ e → Eff r ()
throwSomeExc_ = throwError_ ∘ toException
