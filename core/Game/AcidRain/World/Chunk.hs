{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk
  ( TileOffset(..)
  , Chunk
  , chunkSize
  , chunkHeight

    -- * Constructing chunks
  , new
  , putEntity
  , deleteEntity

    -- * Querying chunks
  , tileStateAt
  , climateAt
#if defined(DEBUG)
  , riverAt
#endif
  , biomeAt
  , entityAt
  , hasEntityAt
  , canEntityEnter
  ) where

import Control.Monad.Catch (MonadThrow)
import Data.Default (Default(..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import qualified Data.Vector.Generic as GV
import Game.AcidRain.World.Biome (Biome(..), SomeBiome)
import Game.AcidRain.World.Biome.Palette (BiomePalette)
import qualified Game.AcidRain.World.Biome.Palette as BPal
import Game.AcidRain.World.Biome.Registry (BiomeRegistry)
import qualified Game.AcidRain.World.Biome.Registry as BR
import Game.AcidRain.World.Chunk.Types
  ( Chunk(..), TileOffset(..), IndexedTileState(..)
  , cTileReg, cTilePal, cTiles
  , cClimates
#if defined(DEBUG)
  , cRivers
#endif
  , cBiomeReg, cBiomePal, cBiomes
  , cEntities, chunkSize, chunkHeight
  , assertValidOffset, assertValidEntity
  , toIndexed )
import Game.AcidRain.World.Climate (Climate)
import Game.AcidRain.World.Entity (Entity(..), SomeEntity)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue)
import Game.AcidRain.World.Tile (TileState(..), SomeTileState, isSolid)
import Game.AcidRain.World.Tile.Palette (TilePalette)
import qualified Game.AcidRain.World.Tile.Palette as TPal
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Lens.Micro ((&), (^.), (%~))
import Prelude.Unicode ((⋅), (∘))


-- | Create a chunk filled with a single specific tile which is
-- usually @acid-rain:air@. It also takes a single specific biome
-- which is usually @acid-rain:plains@.
new ∷ (Biome β, MonadThrow μ)
    ⇒ TileRegistry
    → TilePalette
    → BiomeRegistry
    → BiomePalette
    → EntityCatalogue
    → TileState τ
    → β
    → μ Chunk
new tReg tPal bReg bPal eCat tFill bFill
  = do its  ← toIndexed tPal tFill
       bIdx ← BPal.indexOf (biomeID bFill) bPal
       return $ Chunk
         { _cTileReg  = tReg
         , _cTilePal  = tPal
         , _cTiles    = GV.replicate (chunkSize⋅chunkSize⋅chunkHeight) its
         , _cClimates = GV.replicate (chunkSize⋅chunkSize) def
#if defined(DEBUG)
         , _cRivers   = GV.replicate (chunkSize⋅chunkSize) 0
#endif
         , _cBiomeReg = bReg
         , _cBiomePal = bPal
         , _cBiomes   = GV.replicate (chunkSize⋅chunkSize) bIdx
         , _cEntCat   = eCat
         , _cEntities = HM.empty
         }

-- | Place an entity at a given offset in a chunk.
putEntity ∷ Entity ε ⇒ TileOffset → ε → Chunk → Chunk
putEntity off e c
  = assertValidOffset off $
    assertValidEntity e c $
    c & cEntities %~ HM.insert off (upcastEntity e)

-- | Remove an entity at a given offset in a chunk if any.
deleteEntity ∷ TileOffset → Chunk → Chunk
deleteEntity off c
  = assertValidOffset off $
    c & cEntities %~ HM.delete off

-- | Get the tile state at a given offset in a chunk.
tileStateAt ∷ MonadThrow μ ⇒ TileOffset → Chunk → μ SomeTileState
tileStateAt off@(TileOffset { x, y, z }) c
  = assertValidOffset off $
    let x'  = fromIntegral x ∷ Int
        y'  = fromIntegral y ∷ Int
        z'  = fromIntegral z ∷ Int
        its = (c^.cTiles) GV.! (y'⋅chunkHeight⋅chunkSize + x'⋅chunkHeight + z')
    in
      do tid  ← TPal.idOf (itsIndex its) (c^.cTilePal)
         tile ← TR.get tid (c^.cTileReg)
         return $ TileState
           { tsTile  = tile
           , tsValue = itsValue its
           }

-- | Get the climate at a given @(x, y)@ offset in a chunk.
climateAt ∷ TileOffset → Chunk → Climate
climateAt off@(TileOffset { x, y, .. }) c
  = assertValidOffset off $
    let x' = fromIntegral x ∷ Int
        y' = fromIntegral y ∷ Int
    in
      (c^.cClimates) GV.! (y'⋅chunkSize + x')

#if defined(DEBUG)
-- | Get the river strength at a given @(x, y)@ offset in a chunk.
riverAt ∷ TileOffset → Chunk → Float
riverAt off@(TileOffset { x, y, .. }) c
  = assertValidOffset off $
    let x' = fromIntegral x ∷ Int
        y' = fromIntegral y ∷ Int
    in
      (c^.cRivers) GV.! (y'⋅chunkSize + x')
#endif

-- | Get the biome at a given @(x, y)@ offset in a chunk.
biomeAt ∷ MonadThrow μ ⇒ TileOffset → Chunk → μ SomeBiome
biomeAt off@(TileOffset { x, y, .. }) c
  = assertValidOffset off $
    let x'   = fromIntegral x ∷ Int
        y'   = fromIntegral y ∷ Int
        bIdx = (c^.cBiomes) GV.! (y'⋅chunkSize + x')
    in
      do bid   ← BPal.idOf bIdx (c^.cBiomePal)
         biome ← BR.get bid (c^.cBiomeReg)
         return biome

-- | Lookup an entity possibly located at a given offset in a chunk.
entityAt ∷ TileOffset → Chunk → Maybe SomeEntity
entityAt off c
  = assertValidOffset off $
    HM.lookup off (c^.cEntities)

-- | Return 'True' iff there is an entity at a given offset in a
-- chunk.
hasEntityAt ∷ TileOffset → Chunk → Bool
hasEntityAt = (isJust ∘) ∘ entityAt

-- | Return 'True' iff an entity can enter a given spot in a chunk. An
-- entity can enter a spot iff (a) there is no entity at the spot, and
-- (b) the tile there is not solid.
canEntityEnter ∷ MonadThrow μ ⇒ TileOffset → Chunk → μ Bool
canEntityEnter off c
  = if hasEntityAt off c then
      return False
    else
      do ts ← tileStateAt off c
         return $ not $ isSolid ts
