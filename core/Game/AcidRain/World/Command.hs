{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Command
  ( Command(..)
  , CommandID
  , CommandType(..)
  , SomeCommand(..)
  ) where

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
