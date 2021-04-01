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
  , sendToWorld

    -- * Server-side context
  , IServerCtx(..)
  , ServerCtx
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State, get)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Hashable (Hashable(..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Game.AcidRain.TUI.Keystroke (Keystroke)
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
  -- | Run the command on the client side.
  runOnClient ∷ (MonadIO (Eff r), Member (State ClientCtx) r)
              ⇒ c
              → [Text]
              → Eff r ()
  runOnClient = sendToWorld

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

-------------------------------------------------------------------------------
-- Client-side context
-------------------------------------------------------------------------------

-- | Command-evaluating context for the client side.
class Typeable ctx ⇒ IClientCtx ctx where
  -- This type class cannot mention 'World' because doing so would end
  -- up in a dependency cycle.

  -- | Erase the type of 'IClientCtx'.
  upcastClientCtx ∷ ctx → ClientCtx
  upcastClientCtx = ClientCtx
  -- | Recover the type of 'IClientCtx'.
  downcastClientCtx ∷ ClientCtx → Maybe ctx
  downcastClientCtx (ClientCtx ctx) = cast ctx
  -- | Schedule the command (along with arguments) to run on the world
  -- context. Command implementations should use 'sendToWorld' instead
  -- of invoking this directly.
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
  basicSendToWorld (ClientCtx ctx) = basicSendToWorld ctx

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
-- Server-side context
-------------------------------------------------------------------------------

-- | Command-evaluating and ticking context for the server side.
class Typeable ctx ⇒ IServerCtx ctx where
  -- | Erase the type of 'IServerCtx'.
  upcastServerCtx ∷ ctx → ServerCtx
  upcastServerCtx = ServerCtx
  -- | Recover the type of 'IServerCtx'.
  downcastServerCtx ∷ ServerCtx → Maybe ctx
  downcastServerCtx (ServerCtx ctx) = cast ctx

-- | Type-erased 'IServerCtx'. We hate this for the same reason as
-- 'ClientCtx'.
data ServerCtx = ∀ctx. IServerCtx ctx ⇒ ServerCtx !ctx

instance IServerCtx ServerCtx where
  upcastServerCtx = id
  downcastServerCtx = Just
