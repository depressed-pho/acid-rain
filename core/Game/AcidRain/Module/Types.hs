{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Types
  ( ModuleID
  , Module(..)
  , SomeModule(..)
  , ModuleMap
  , LoaderContext(..)
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadState)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Game.AcidRain.World.Tile.Registry (TileRegistry)

-- | A module ID is used for things like prefixes of tiles such as
-- @acid-rain:dirt@. @acid-rain@ is the module ID in this case.
type ModuleID = Text

class Module α where
  -- | Get the ID of the module.
  modID ∷ α → ModuleID
  -- | Load the module. Note that modules are loaded per world, not
  -- per the entire process. When a module is to be loaded, it is
  -- guaranteed that all the modules it depends on have already been
  -- loaded. FIXME: dependency resolution
  load ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ α → μ ()

-- | A type-erased 'Module'.
data SomeModule = ∀α. Module α ⇒ SomeModule α

instance Module SomeModule where
  modID (SomeModule m) = modID m
  load (SomeModule m) = load m

-- | A collection of modules, indexed by their IDs.
type ModuleMap = HashMap ModuleID SomeModule

data LoaderContext
  = LoaderContext
    { -- | Modules that have been fully loaded. This doesn't include a
      -- module that is currently being loaded.
      lcMods  ∷ ModuleMap
      -- | The tile registry that is being constructed.
    , lcTiles ∷ TileRegistry
    }
