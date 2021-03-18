{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Loader
  ( LoaderContext

  -- * Registering tiles
  , getTileRegistry
  , putTileRegistry
  , registerTile
  , lookupTile
  , getTile

  -- * Registering entity types
  , getEntityRegistry
  , putEntityRegistry
  , registerEntityType
  , lookupEntityType
  , getEntityType

  -- * Loading modules
  , ModuleMap
  , loadModules
  , lcMods
  , lcTiles
  , lcEntityTypes
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadState, execStateT, modify', gets)
import Data.Foldable (traverse_, toList)
import qualified Data.HashMap.Strict as HM
import Game.AcidRain.Module.Types (Module(..), SomeModule(..), ModuleMap, LoaderContext(..))
import Game.AcidRain.World.Entity (EntityType, EntityTypeID, SomeEntityType)
import Game.AcidRain.World.Entity.Registry (EntityRegistry)
import qualified Game.AcidRain.World.Entity.Registry as ER
import Game.AcidRain.World.Tile (Tile, TileID, SomeTile)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Prelude.Unicode ((∘))

-- | Create a new, empty context to begin with.
empty ∷ LoaderContext
empty
  = LoaderContext
    { lcMods        = HM.empty
    , lcTiles       = TR.empty
    , lcEntityTypes = ER.empty
    }

-- | Load a single module.
loadMod ∷ (Module α, MonadState LoaderContext μ, MonadThrow μ) ⇒ α → μ ()
loadMod m
  = do load m
       modify' $ \ctx → ctx { lcMods = insMod' $ lcMods ctx }
  where
    insMod' ∷ ModuleMap → ModuleMap
    insMod' = HM.insert (modID m) (upcastModule m)

-- | Load a set of modules. While this function takes a sequence of
-- modules, its order does not determine the actual order of modules
-- to be loaded. It's just because 'Module' is not 'Ord'.
--
-- The function first reorders modules based on their dependencies,
-- and loads each module one at a time. Missing dependencies or cycles
-- are reported by throwing exceptions.
--
-- This function only makes sense for client and server
-- implementations. Modules shouldn't use this.
loadModules ∷ (Foldable f, MonadThrow μ) ⇒ f SomeModule → μ LoaderContext
loadModules mods
  = do mods' ← reorderMods mods
       execStateT (traverse_ loadMod mods') empty

reorderMods ∷ (Foldable f, MonadThrow μ) ⇒ f SomeModule → μ [SomeModule]
reorderMods = return . toList -- FIXME: Actually reorder it.

-- | Get the tile registry from the context. Module loaders usually
-- don't need to use this directly. There are helper functions such as
-- 'registerTile' to manipulate the tile registry in a loader.
getTileRegistry ∷ MonadState LoaderContext μ ⇒ μ TileRegistry
getTileRegistry = gets lcTiles

-- | Put a tile registry to the context. Module loaders usually don't
-- need to use this directly.
putTileRegistry ∷ MonadState LoaderContext μ ⇒ TileRegistry → μ ()
putTileRegistry tiles
  = modify' $ \ctx → ctx { lcTiles = tiles }

-- | Register a tile. Throws if it's already been registered.
registerTile ∷ (MonadState LoaderContext μ, MonadThrow μ, Tile τ) ⇒ τ → μ ()
registerTile tile
  = getTileRegistry >>= TR.register tile >>= putTileRegistry

-- | Lookup a tile by its ID.
lookupTile ∷ (MonadState LoaderContext μ) ⇒ TileID → μ (Maybe SomeTile)
lookupTile tid
  = getTileRegistry >>= return ∘ TR.lookup tid

-- | Get a tile by its ID. Throws if it doesn't exist.
getTile ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ TileID → μ SomeTile
getTile tid
  = getTileRegistry >>= TR.get tid

-- | Get the entity registry from the context. Module loaders usually
-- don't need to use this directly. There are helper functions such as
-- 'registerEntityType' to manipulate the entity registry in a loader.
getEntityRegistry ∷ MonadState LoaderContext μ ⇒ μ EntityRegistry
getEntityRegistry = gets lcEntityTypes

-- | Put an entity registry to the context. Module loaders usually
-- don't need to use this directly.
putEntityRegistry ∷ MonadState LoaderContext μ ⇒ EntityRegistry → μ ()
putEntityRegistry entities
  = modify' $ \ctx → ctx { lcEntityTypes = entities }

-- | Register an entity type. Throws if it's already been registered.
registerEntityType ∷ (MonadState LoaderContext μ, MonadThrow μ, EntityType τ) ⇒ τ → μ ()
registerEntityType et
  = getEntityRegistry >>= ER.register et >>= putEntityRegistry

-- | Lookup an entity type by its ID.
lookupEntityType ∷ (MonadState LoaderContext μ) ⇒ EntityTypeID → μ (Maybe SomeEntityType)
lookupEntityType etid
  = getEntityRegistry >>= return ∘ ER.lookup etid

-- | Get an entity type by its ID. Throws if it doesn't exist.
getEntityType ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ EntityTypeID → μ SomeEntityType
getEntityType etid
  = getEntityRegistry >>= ER.get etid
