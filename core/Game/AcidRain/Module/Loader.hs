{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Loader
  ( LoaderContext

    -- * Querying world configuration
  , getWorldSeed

    -- * Registering tiles
  , getTileRegistry
  , putTileRegistry
  , registerTile
  , lookupTile
  , getTile

    -- * Registering biomes
  , getBiomeRegistry
  , putBiomeRegistry
  , registerBiome
  , lookupBiome
  , getBiome

    -- * Registering entity types
  , getEntityRegistry
  , putEntityRegistry
  , registerEntityType
  , lookupEntityType
  , getEntityType

    -- * Registering commands
  , getCommandRegistry
  , putCommandRegistry
  , registerCommand
  , lookupCommand
  , getCommand

    -- * Modifying chunk generator
  , modifyChunkGenerator

    -- * Loading modules
  , ModuleMap
  , loadModules
  , lcWorldSeed
  , lcMods
  , lcTileReg
  , lcBiomeReg
  , lcEntityReg
  , lcCommandReg
  , lcChunkGen
  ) where

import Control.Eff (Eff, Member, runLift)
import Control.Eff.Instances.Catch ()
import Control.Eff.State.Strict (State, execState, get, modify)
import Control.Monad.Catch (MonadThrow)
import Data.Default (Default(..))
import Data.Foldable (traverse_, toList)
import qualified Data.HashMap.Strict as HM
import Data.Poly.Strict (Poly)
import Game.AcidRain.Module.Types
  ( Module(..), ModuleMap, LoaderContext(..)
  , lcWorldSeed, lcMods, lcTileReg, lcBiomeReg
  , lcEntityReg, lcCommandReg, lcChunkGen )
import Game.AcidRain.World (WorldSeed, Command, CommandID)
import Game.AcidRain.World.Biome (Biome, BiomeID)
import Game.AcidRain.World.Biome.Registry (BiomeRegistry)
import qualified Game.AcidRain.World.Biome.Registry as BR
import Game.AcidRain.World.Chunk.Generator (ChunkGenerator)
import Game.AcidRain.World.Command.Registry (CommandRegistry)
import qualified Game.AcidRain.World.Command.Registry as CR
import Game.AcidRain.World.Entity (EntityType, EntityTypeID)
import Game.AcidRain.World.Entity.Registry (EntityRegistry)
import qualified Game.AcidRain.World.Entity.Registry as ER
import Game.AcidRain.World.Tile (Tile, TileID)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Lens.Micro ((%~), (.~))
import Prelude.Unicode ((∘))

-- Create a new, empty context to begin with.
empty ∷ WorldSeed → LoaderContext
empty seed
  = LoaderContext
    { _lcWorldSeed  = seed
    , _lcMods       = HM.empty
    , _lcTileReg    = TR.empty
    , _lcBiomeReg   = BR.empty
    , _lcEntityReg  = ER.empty
    , _lcCommandReg = CR.empty
    , _lcChunkGen   = def
    }

-- | Load a single module.
loadMod ∷ (Module α, Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ α → Eff r ()
loadMod m
  = do load m
       modify $ lcMods %~ insMod'
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
loadModules ∷ (Foldable f, MonadThrow μ)
            ⇒ f (Poly Module)
            → WorldSeed
            → μ LoaderContext
loadModules mods seed
  = do mods' ← reorderMods mods
       runLift $ execState (empty seed) (traverse_ loadMod mods')

reorderMods ∷ (Foldable f, MonadThrow μ) ⇒ f (Poly Module) → μ [Poly Module]
reorderMods = return . toList -- FIXME: Actually reorder it.

-- | Get the seed of which the world that the modules are being loaded
-- for.
getWorldSeed ∷ Member (State LoaderContext) r ⇒ Eff r WorldSeed
getWorldSeed = _lcWorldSeed <$> get

-- | Get the tile registry from the context. Module loaders usually
-- don't need to use this directly. There are helper functions such as
-- 'registerTile' to manipulate the tile registry in a loader.
getTileRegistry ∷ Member (State LoaderContext) r ⇒ Eff r TileRegistry
getTileRegistry = _lcTileReg <$> get

-- | Put a tile registry to the context. Module loaders usually don't
-- need to use this directly.
putTileRegistry ∷ Member (State LoaderContext) r ⇒ TileRegistry → Eff r ()
putTileRegistry tiles
  = modify $ lcTileReg .~ tiles

-- | Register a tile. Throws if it's already been registered.
registerTile ∷ (Tile τ, Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ τ → Eff r ()
registerTile tile
  = getTileRegistry >>= TR.register tile >>= putTileRegistry

-- | Lookup a tile by its ID.
lookupTile ∷ Member (State LoaderContext) r ⇒ TileID → Eff r (Maybe (Poly Tile))
lookupTile tid
  = getTileRegistry >>= return ∘ TR.lookup tid

-- | Get a tile by its ID. Throws if it doesn't exist.
getTile ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ TileID → Eff r (Poly Tile)
getTile tid
  = getTileRegistry >>= TR.get tid

-- | Get the biome registry from the context. Module loaders usually
-- don't need to use this directly. There are helper functions such as
-- 'registerBiome' to manipulate the biome registry in a loader.
getBiomeRegistry ∷ Member (State LoaderContext) r ⇒ Eff r BiomeRegistry
getBiomeRegistry = _lcBiomeReg <$> get

-- | Put a biome registry to the context. Module loaders usually don't
-- need to use this directly.
putBiomeRegistry ∷ Member (State LoaderContext) r ⇒ BiomeRegistry → Eff r ()
putBiomeRegistry biomes
  = modify $ lcBiomeReg .~ biomes

-- | Register a biome. Throws if it's already been registered.
registerBiome ∷ (Biome β, Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ β → Eff r ()
registerBiome biome
  = getBiomeRegistry >>= BR.register biome >>= putBiomeRegistry

-- | Lookup a biome by its ID.
lookupBiome ∷ Member (State LoaderContext) r ⇒ BiomeID → Eff r (Maybe (Poly Biome))
lookupBiome bid
  = getBiomeRegistry >>= return ∘ BR.lookup bid

-- | Get a biome by its ID. Throws if it doesn't exist.
getBiome ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ BiomeID → Eff r (Poly Biome)
getBiome bid
  = getBiomeRegistry >>= BR.get bid

-- | Get the entity registry from the context. Module loaders usually
-- don't need to use this directly. There are helper functions such as
-- 'registerEntityType' to manipulate the entity registry in a loader.
getEntityRegistry ∷ Member (State LoaderContext) r ⇒ Eff r EntityRegistry
getEntityRegistry = _lcEntityReg <$> get

-- | Put an entity registry to the context. Module loaders usually
-- don't need to use this directly.
putEntityRegistry ∷ Member (State LoaderContext) r ⇒ EntityRegistry → Eff r ()
putEntityRegistry entities
  = modify $ lcEntityReg .~ entities

-- | Register an entity type. Throws if it's already been registered.
registerEntityType ∷ (EntityType τ, Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ τ → Eff r ()
registerEntityType et
  = getEntityRegistry >>= ER.register et >>= putEntityRegistry

-- | Lookup an entity type by its ID.
lookupEntityType ∷ Member (State LoaderContext) r ⇒ EntityTypeID → Eff r (Maybe (Poly EntityType))
lookupEntityType etid
  = getEntityRegistry >>= return ∘ ER.lookup etid

-- | Get an entity type by its ID. Throws if it doesn't exist.
getEntityType ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ EntityTypeID → Eff r (Poly EntityType)
getEntityType etid
  = getEntityRegistry >>= ER.get etid

-- | Get the command registry from the context. Module loaders usually
-- don't need to use this directly. There are helper functions such as
-- 'registerCommand' to manipulate the command registry in a loader.
getCommandRegistry ∷ Member (State LoaderContext) r ⇒ Eff r CommandRegistry
getCommandRegistry = _lcCommandReg <$> get

-- | Put a command registry to the context. Module loaders usually don't
-- need to use this directly.
putCommandRegistry ∷ Member (State LoaderContext) r ⇒ CommandRegistry → Eff r ()
putCommandRegistry commands
  = modify $ lcCommandReg .~ commands

-- | Register a command. Throws if it's already been registered.
registerCommand ∷ (Command c, Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ c → Eff r ()
registerCommand command
  = getCommandRegistry >>= CR.register command >>= putCommandRegistry

-- | Lookup a command by its ID.
lookupCommand ∷ Member (State LoaderContext) r ⇒ CommandID → Eff r (Maybe (Poly Command))
lookupCommand cid
  = getCommandRegistry >>= return ∘ CR.lookup cid

-- | Get a command by its ID. Throws if it doesn't exist.
getCommand ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ CommandID → Eff r (Poly Command)
getCommand cid
  = getCommandRegistry >>= CR.get cid

-- | Modify the chunk generator by applying a given function.
modifyChunkGenerator ∷ Member (State LoaderContext) r ⇒ (ChunkGenerator → ChunkGenerator) → Eff r ()
modifyChunkGenerator f
  = modify $ lcChunkGen %~ f
