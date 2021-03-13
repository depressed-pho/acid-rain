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
  -- | Erase the type of the module.
  upcastModule ∷ α → SomeModule
  upcastModule = SomeModule
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
  upcastModule = id
  modID (SomeModule m) = modID m
  load (SomeModule m) = load m

-- | A collection of modules, indexed by their IDs.
type ModuleMap = HashMap ModuleID SomeModule

-- | An opaque data structure representing a state of module loading.
data LoaderContext
  = LoaderContext
    { -- | Extract modules that have been fully loaded. This doesn't
      -- include a module that is currently being loaded.
      lcMods  ∷ !ModuleMap
      -- | Extract the tile registry that has been constructed.
    , lcTiles ∷ !TileRegistry
    }
