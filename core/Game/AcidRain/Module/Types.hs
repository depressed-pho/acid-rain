{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Types
  ( ModuleID
  , Module(..)
  , SomeModule(..)
  , ModuleMap
  , LoaderContext(..)
  , lcWorldSeed, lcMods, lcTiles, lcBiomes, lcEntityTypes, lcChunkGen
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Game.AcidRain.World (WorldSeed)
import Game.AcidRain.World.Biome.Registry (BiomeRegistry)
import Game.AcidRain.World.Chunk.Generator (ChunkGenerator)
import Game.AcidRain.World.Entity.Registry (EntityRegistry)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import Lens.Micro.TH (makeLenses)

-- | A module ID is used for things like prefixes of tiles such as
-- @acid-rain:dirt@. @acid-rain@ is the module ID in this case.
type ModuleID = Text

-- | An instance of this class defines a module of the game. A module
-- is a collection of assets such as tiles, recipes, world-gen hooks,
-- and so on. An instance of 'Module' typically has no values, and
-- instead implements the class on 'Data.Proxy.Proxy' like:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- data BuiltinModule
-- instance 'Module' ('Data.Proxy.Proxy' BuiltinModule) where
--   modID _ = "acid-rain"
--   ..
-- @
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
  load ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ α → Eff r ()

-- | A type-erased 'Module'.
data SomeModule = ∀α. Module α ⇒ SomeModule !α

instance Module SomeModule where
  upcastModule = id
  modID (SomeModule m) = modID m
  load (SomeModule m) = load m

-- | A collection of modules, indexed by their IDs.
type ModuleMap = HashMap ModuleID SomeModule

-- | An opaque data structure representing a state of module loading.
data LoaderContext
  = LoaderContext
    { -- | Extract the world seed of which the world that the modules
      -- are being loaded for.
      _lcWorldSeed   ∷ !WorldSeed
      -- | Extract modules that have been fully loaded. This doesn't
      -- include a module that is currently being loaded.
    , _lcMods        ∷ !ModuleMap
      -- | Extract the tile registry that has been constructed.
    , _lcTiles       ∷ !TileRegistry
      -- | Extract the biome registry that has been constructed.
    , _lcBiomes      ∷ !BiomeRegistry
      -- | Extract the entity registry that has been constructed.
    , _lcEntityTypes ∷ !EntityRegistry
      -- | Extract the chunk generator that has been composed.
    , _lcChunkGen    ∷ !ChunkGenerator
    }

makeLenses ''LoaderContext
