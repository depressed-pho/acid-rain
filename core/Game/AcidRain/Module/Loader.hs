{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Loader
  ( LoaderContext

  -- * Registering tiles
  , registerTile
  , lookupTile
  , getTile

  -- * Loading modules
  , loadModules
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadState, execStateT, modify', gets)
import Data.Foldable (traverse_, toList)
import qualified Data.HashMap.Strict as HM
import Game.AcidRain.Module.Types (Module(..), SomeModule(..), ModuleMap, LoaderContext(..))
import Game.AcidRain.World.Tile (Tile, TileID, SomeTile)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Prelude.Unicode ((∘))

-- Create a new, empty context to begin with.
empty ∷ LoaderContext
empty
  = LoaderContext
    { lcMods  = HM.empty
    , lcTiles = TR.empty
    }

-- Load a single module.
loadMod ∷ (Module α, MonadState LoaderContext μ, MonadThrow μ) ⇒ α → μ ()
loadMod m
  = do load m
       modify' $ \ctx → ctx { lcMods = insMod' $ lcMods ctx }
  where
    insMod' ∷ ModuleMap → ModuleMap
    insMod' = HM.insert (modID m) (SomeModule m)

-- | Load a set of modules. While this function takes a sequence of
-- modules, its order does not determine the actual order of modules
-- to be loaded. It's just because 'Module' is not 'Ord'.
--
-- The function first reorders modules based on their dependencies,
-- and loads each module one at a time. Missing dependencies or cycles
-- are reported by throwing exceptions.
--
-- This function only makes sense for clients and servers. Modules
-- shouldn't use this.
loadModules ∷ (Foldable f, MonadThrow μ) ⇒ f SomeModule → μ LoaderContext
loadModules mods
  = do mods' ← reorderMods mods
       execStateT (traverse_ loadSomeMod mods') empty
  where
    loadSomeMod (SomeModule m) = loadMod m

reorderMods ∷ (Foldable f, MonadThrow μ) ⇒ f SomeModule → μ [SomeModule]
reorderMods = return . toList -- FIXME: Actually reorder it.

-- | Register a tile. Throws if it's already been registered.
registerTile ∷ (MonadState LoaderContext μ, MonadThrow μ, Tile τ) ⇒ τ → μ ()
registerTile tile
  = do tiles  ← gets lcTiles
       tiles' ← TR.register tile tiles
       modify' $ \ctx → ctx { lcTiles = tiles' }

-- | Lookup a tile by its ID.
lookupTile ∷ (MonadState LoaderContext μ) ⇒ TileID → μ (Maybe SomeTile)
lookupTile tid
  = gets lcTiles >>= return ∘ TR.lookup tid

-- | Get a tile by its ID. Throws if it doesn't exist.
getTile ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ TileID → μ SomeTile
getTile tid
  = gets lcTiles >>= TR.get tid
