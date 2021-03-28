{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Command.Registry
  ( -- * The registry type
    CommandRegistry

    -- * Constructing registries
  , empty

    -- * Manipulating registries
  , register

    -- * Querying registries
  , lookup
  , get

    -- * Exceptions
  , ConflictingCommandIDException(..)
  , UnknownCommandIDException(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.MonoTraversable
  ( Element, MonoFunctor, MonoFoldable, MonoTraversable, GrowingAppend
  , otraverse )
import Game.AcidRain.World.Command (Command(..), CommandID, SomeCommand(..))
import Prelude hiding (lookup)


-- | The command registry is a data structure that contains immutable
-- 'Command' objects. It is constructed while loading a world, and
-- becomes immutable afterwards.
newtype CommandRegistry = CommandRegistry (HashMap CommandID SomeCommand)
  deriving ( Show, MonoFunctor, MonoFoldable, GrowingAppend, Semigroup
           , Monoid )

-- GeneralisedNewtypeDeriving can't derive MonoTraversable for us due
-- to a limitation of the compiler. So we do it manually.
instance MonoTraversable CommandRegistry where
  otraverse f (CommandRegistry reg)
    = CommandRegistry <$> traverse f reg

type instance Element CommandRegistry = SomeCommand

-- | Create an empty registry.
empty ∷ CommandRegistry
empty = CommandRegistry HM.empty

-- | Register a command to the registry. Throws if it's already been
-- registered.
register ∷ (Command β, MonadThrow μ) ⇒ β → CommandRegistry → μ CommandRegistry
register command (CommandRegistry reg)
  = let cid = commandID command
    in
      case HM.member cid reg of
        True  → throwM $ ConflictingCommandIDException cid
        False → return $ CommandRegistry $ HM.insert cid (upcastCommand command) reg

-- | Lookup a command by its ID. Return 'Nothing' if no commands matching
-- with the given ID has been registered.
lookup ∷ CommandID → CommandRegistry → Maybe SomeCommand
lookup cid (CommandRegistry reg)
  = HM.lookup cid reg

-- | Get a command by its ID. Throws if it doesn't exist.
get ∷ MonadThrow μ ⇒ CommandID → CommandRegistry → μ SomeCommand
get cid reg
  = case lookup cid reg of
      Just command → return command
      Nothing      → throwM $ UnknownCommandIDException cid

-- | An exception to be thrown when two commands with the same ID is
-- being registered.
data ConflictingCommandIDException = ConflictingCommandIDException !CommandID
  deriving Show

instance Exception ConflictingCommandIDException

-- | An exception to be thrown when there was no command having the given
-- ID.
data UnknownCommandIDException = UnknownCommandIDException !CommandID
  deriving Show

instance Exception UnknownCommandIDException