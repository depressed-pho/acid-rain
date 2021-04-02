{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Command
  ( Command(..)
  , CommandID
  , CommandType(..)
  , SomeCommand(..)

    -- * Client-side context
  , IClientCtx(..)
  , ClientCtx
  , getClientPlayer
  , sendToWorld

    -- * World-side context
  , IWorldCtx(..)
  , WorldCtx
  , reportWorldCommandError
  ) where

import Control.Eff (Eff, Lifted, Member)
import Control.Eff.Reader.Lazy (Reader)
import Control.Eff.State.Strict (State, get)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.STM (STM)
import Data.Hashable (Hashable(..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Game.AcidRain.TUI.Keystroke (Keystroke)
import Game.AcidRain.World.Player (PlayerID)
import Prelude.Unicode ((≡))


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
  runOnClient ∷ (MonadIO (Eff r), Member (State ClientCtx) r)
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
  -- Throwing exceptions in this method would result in a server
  -- crash. It should therefore be avoided unless something
  -- catastrophic happens.
  runOnWorld ∷ (Lifted STM r, Member (Reader WorldCtx) r)
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
  runOnWorld (SomeCommand c) = runOnWorld c

-------------------------------------------------------------------------------
-- Client-side context
-------------------------------------------------------------------------------

-- | Command-evaluating context for the client side. Command
-- implementations should not invoke its methods directly.
class Typeable ctx ⇒ IClientCtx ctx where
  -- This type class cannot mention 'World' because doing so would end
  -- up in a dependency cycle.

  -- | Erase the type of 'IClientCtx'.
  upcastClientCtx ∷ ctx → ClientCtx
  upcastClientCtx = ClientCtx
  -- | Recover the type of 'IClientCtx'.
  downcastClientCtx ∷ ClientCtx → Maybe ctx
  downcastClientCtx (ClientCtx ctx) = cast ctx
  -- | Get the ID of the player whom the client controls.
  basicGetClientPlayer ∷ ctx → PlayerID
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
  basicGetClientPlayer (ClientCtx ctx) = basicGetClientPlayer ctx
  basicSendToWorld (ClientCtx ctx) = basicSendToWorld ctx

-- | Get the ID of the player whom the client controls.
getClientPlayer ∷ Member (State ClientCtx) r ⇒ Eff r PlayerID
getClientPlayer
  = do ctx ← get
       return $ basicGetClientPlayer (ctx ∷ ClientCtx)

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

-- | Type-erased 'IWorldCtx'. We hate this for the same reason as
-- 'ClientCtx'.
data WorldCtx = ∀ctx. IWorldCtx ctx ⇒ WorldCtx !ctx

instance IWorldCtx WorldCtx where
  upcastWorldCtx = id
  downcastWorldCtx = Just

reportWorldCommandError ∷ (Command c, Lifted STM r, Member (Reader WorldCtx) r)
                        ⇒ c
                        → Maybe PlayerID
                        → [Text]
                        → Eff r ()
reportWorldCommandError cmd mPid args
  = error ("FIXME: command error: " ++ show cmd ++ " " ++ show mPid ++ " " ++ show args)
