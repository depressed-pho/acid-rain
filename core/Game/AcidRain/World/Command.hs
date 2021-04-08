{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Command
  ( Command(..)
  , CommandID
  , CommandType(..)
  , SomeCommand(..)

    -- * Errors while running commands
  , BadArgumentsException(..)
  , ClientOnlyCommandException(..)
  , throwSomeExc
  , throwSomeExc_

    -- * Client-side context
  , IClientCtx(..)
  , ClientCtx
  , getClientPlayerID
  , sendToWorld

    -- * World-side context
  , IWorldCtx(..)
  , WorldCtx
  , fireEvent
  , getPlayer
  , modifyPlayer
  , tryMoveEntity
  ) where

import Control.Eff (Eff, Lifted, Member, type(<::))
import Control.Eff.Exception (Exc, throwError, throwError_)
import Control.Eff.State.Strict (State, get, put)
import Control.Exception (Exception(..), SomeException, toException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.STM (STM)
import Data.Hashable (Hashable(..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Game.AcidRain.TUI.Keystroke (Keystroke)
import Game.AcidRain.World.Event (Event)
import Game.AcidRain.World.Player (Player, PlayerID)
import Game.AcidRain.World.Position (WorldPos)
import Prelude.Unicode ((∘), (≡))


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
-- Errors while running commands
-------------------------------------------------------------------------------
data BadArgumentsException = BadArgumentsException deriving
  (Show, Exception)

-- | Attempted to run a client-only command on the world-side.
data ClientOnlyCommandException = ClientOnlyCommandException
  deriving (Show, Exception)

throwSomeExc ∷ (Exception e, Member (Exc SomeException) r) ⇒ e → Eff r a
throwSomeExc = throwError ∘ toException

throwSomeExc_ ∷ (Exception e, Member (Exc SomeException) r) ⇒ e → Eff r ()
throwSomeExc_ = throwError_ ∘ toException

-------------------------------------------------------------------------------
-- Client-side context
-------------------------------------------------------------------------------

-- | Command-evaluating context for the client side. Command
-- implementations should not invoke its methods directly.
class Typeable ctx ⇒ IClientCtx ctx where
  -- This type class cannot mention 'World' because doing so would end
  -- up with a dependency cycle.

  -- | Erase the type of 'IClientCtx'.
  upcastClientCtx ∷ ctx → ClientCtx
  upcastClientCtx = ClientCtx
  -- | Recover the type of 'IClientCtx'.
  downcastClientCtx ∷ ClientCtx → Maybe ctx
  downcastClientCtx (ClientCtx ctx) = cast ctx
  -- | Get the ID of the player whom the client controls.
  basicGetClientPlayerID ∷ ctx → PlayerID
  -- | Schedule the command (along with arguments) to run on the world
  -- context.
  basicSendToWorld ∷ (Command c, MonadIO μ) ⇒ ctx → c → [Text] → μ ()

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
  basicGetClientPlayerID (ClientCtx ctx) = basicGetClientPlayerID ctx
  basicSendToWorld (ClientCtx ctx) = basicSendToWorld ctx

-- | Get the ID of the player whom the client controls.
getClientPlayerID ∷ Member (State ClientCtx) r ⇒ Eff r PlayerID
getClientPlayerID
  = do ctx ← get
       return $ basicGetClientPlayerID (ctx ∷ ClientCtx)

-- | Schedule the command (along with arguments) to run on the world
-- context.
sendToWorld ∷ (MonadIO (Eff r), Member (State ClientCtx) r, Command c)
            ⇒ c
            → [Text]
            → Eff r ()
sendToWorld c args
  = do ctx ← get
       liftIO $ basicSendToWorld (ctx ∷ ClientCtx) c args

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
