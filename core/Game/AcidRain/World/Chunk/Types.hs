{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk.Types
  ( TileOffset(..)
  , chunkSize
  , chunkHeight

  , assertValidOffset
  , assertValidEntity

  , IndexedTileState(..)
  , Chunk(..)
  , cTileReg, cTilePal, cTiles, cClimates, cBiomeReg, cBiomePal
#if defined(DEBUG)
  , cRivers
#endif
  , cBiomes, cEntCat, cEntities
  , MutableChunk(..)
  , mcTileReg, mcTilePal, mcTiles, mcClimates, mcBiomeReg
#if defined(DEBUG)
  , mcRivers
#endif
  , mcBiomePal, mcBiomes, mcEntCat, mcEntities

  , toIndexed

  , freezeChunk
  , thawChunk

  , writeTileState
  , writeClimate
#if defined(DEBUG)
  , writeRiver
#endif
  , writeBiome
  ) where

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Convertible.Base (Convertible(..))
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Game.AcidRain.World.Biome (Biome(..))
import Game.AcidRain.World.Biome.Palette (BiomePalette, BiomeIndex)
import qualified Game.AcidRain.World.Biome.Palette as BPal
import Game.AcidRain.World.Biome.Registry (BiomeRegistry)
import Game.AcidRain.World.Climate (Climate)
import Game.AcidRain.World.Entity (EntityType(..), Entity(..), SomeEntity)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue, (∈))
import Game.AcidRain.World.Position (WorldPos(..), wpX, wpY, wpZ)
import Game.AcidRain.World.Tile (Tile(..), TileState(..), TileStateValue)
import Game.AcidRain.World.Tile.Palette (TilePalette, TileIndex)
import qualified Game.AcidRain.World.Tile.Palette as TPal
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.))
import Prelude.Unicode ((∘), (⋅))


-- | Unlike Minecraft our chunks are only two blocks tall so we can
-- use a larger chunk size than 16*16.
chunkSize ∷ Integral i ⇒ i
chunkSize = 32

chunkHeight ∷ Integral i ⇒ i
chunkHeight = 2

-- | This is an offset to a tile in a chunk, convertible from 'WorldPos'.
data TileOffset
  = TileOffset
    { x ∷ {-# UNPACK #-} !Word8
    , y ∷ {-# UNPACK #-} !Word8
    , z ∷ {-# UNPACK #-} !Word8
    } deriving (Show, Eq, Generic)

instance Hashable TileOffset

instance Convertible WorldPos TileOffset where
  safeConvert wp
    = Right $ TileOffset
      { x = fromIntegral $ wp^.wpX `mod` chunkSize
      , y = fromIntegral $ wp^.wpY `mod` chunkSize
      , z = fromIntegral $ wp^.wpZ `mod` chunkHeight
      }

-- | This is a variant of 'TileState' which has 'TileIndex' instead of
-- Tile itself. This representation is used both on disk and in memory
-- chunk data to save space.
data IndexedTileState
  = IndexedTileState
    { itsIndex ∷ {-# UNPACK #-} !TileIndex
    , itsValue ∷ {-# UNPACK #-} !TileStateValue
    } deriving (Show, Eq)

-- Derive unboxed vectors for IndexedTileState.
derivingUnbox "ITS"
  [t| IndexedTileState → (TileIndex, TileStateValue) |]
  [e| \its        → (itsIndex its, itsValue its) |]
  [e| \(idx, val) → IndexedTileState idx val     |]

data Chunk
  = Chunk
    { _cTileReg  ∷ !TileRegistry
    , _cTilePal  ∷ !TilePalette
    , _cTiles    ∷ !(UV.Vector IndexedTileState)
    , _cClimates ∷ !(UV.Vector Climate)
#if defined(DEBUG)
    , _cRivers   ∷ !(UV.Vector Float)
#endif
    , _cBiomeReg ∷ !BiomeRegistry
    , _cBiomePal ∷ !BiomePalette
    , _cBiomes   ∷ !(UV.Vector BiomeIndex)
    , _cEntCat   ∷ !EntityCatalogue
    , _cEntities ∷ !(HashMap TileOffset SomeEntity)
    }

makeLenses ''Chunk

instance Show Chunk where
  showsPrec d c
    = showParen (d > appPrec) $
      showString "Chunk " ∘
      showString "{ _cTileReg = "  ∘ showsPrec (appPrec + 1) (c^.cTileReg ) ∘
      showString ", _cTilePal = "  ∘ showsPrec (appPrec + 1) (c^.cTilePal ) ∘
      showString ", _cBiomeReg = " ∘ showsPrec (appPrec + 1) (c^.cBiomeReg) ∘
      showString ", _cBiomePal = " ∘ showsPrec (appPrec + 1) (c^.cBiomePal) ∘
      showString ", _cEntities = " ∘ showsPrec (appPrec + 1) (c^.cEntities) ∘
      showString ", .. }"
    where
      appPrec = 10

data MutableChunk σ
  = MutableChunk
    { _mcTileReg  ∷ !TileRegistry
    , _mcTilePal  ∷ !TilePalette
    , _mcTiles    ∷ !(UV.MVector σ IndexedTileState)
    , _mcClimates ∷ !(UV.MVector σ Climate)
#if defined(DEBUG)
    , _mcRivers   ∷ !(UV.MVector σ Float)
#endif
    , _mcBiomeReg ∷ !BiomeRegistry
    , _mcBiomePal ∷ !BiomePalette
    , _mcBiomes   ∷ !(UV.MVector σ BiomeIndex)
    , _mcEntCat   ∷ !EntityCatalogue
    , _mcEntities ∷ !(HashMap TileOffset SomeEntity)
    }

makeLenses ''MutableChunk

-- | Assert that the given offset is valid.
assertValidOffset ∷ TileOffset → α → α
assertValidOffset (TileOffset { x, y, z })
  = assert (x < chunkSize) ∘
    assert (y < chunkSize) ∘
    assert (z < chunkHeight)

-- | Assert that the given entity is in the catalogue.
assertValidEntity ∷ Entity ε ⇒ ε → Chunk → α → α
assertValidEntity e c
  = assert (entityTypeID (entityType e) ∈ c^.cEntCat)

toIndexed ∷ MonadThrow μ ⇒ TilePalette → TileState τ → μ IndexedTileState
toIndexed palette (TileState { tsTile, tsValue })
  = do idx ← TPal.indexOf (tileID tsTile) palette
       return $ IndexedTileState
         { itsIndex = idx
         , itsValue = tsValue
         }

freezeChunk ∷ PrimMonad μ ⇒ MutableChunk (PrimState μ) → μ Chunk
freezeChunk mc
  = do tiles    ← GV.freeze $ mc^.mcTiles
       climates ← GV.freeze $ mc^.mcClimates
#if defined(DEBUG)
       rivers   ← GV.freeze $ mc^.mcRivers
#endif
       biomes   ← GV.freeze $ mc^.mcBiomes
       return Chunk
         { _cTileReg  = mc^.mcTileReg
         , _cTilePal  = mc^.mcTilePal
         , _cTiles    = tiles
         , _cClimates = climates
#if defined(DEBUG)
         , _cRivers   = rivers
#endif
         , _cBiomeReg = mc^.mcBiomeReg
         , _cBiomePal = mc^.mcBiomePal
         , _cBiomes   = biomes
         , _cEntCat   = mc^.mcEntCat
         , _cEntities = mc^.mcEntities
         }

thawChunk ∷ PrimMonad μ ⇒ Chunk → μ (MutableChunk (PrimState μ))
thawChunk c
  = do tiles    ← GV.thaw $ c^.cTiles
       climates ← GV.thaw $ c^.cClimates
#if defined(DEBUG)
       rivers   ← GV.thaw $ c^.cRivers
#endif
       biomes   ← GV.thaw $ c^.cBiomes
       return MutableChunk
         { _mcTileReg  = c^.cTileReg
         , _mcTilePal  = c^.cTilePal
         , _mcTiles    = tiles
         , _mcClimates = climates
#if defined(DEBUG)
         , _mcRivers   = rivers
#endif
         , _mcBiomeReg = c^.cBiomeReg
         , _mcBiomePal = c^.cBiomePal
         , _mcBiomes   = biomes
         , _mcEntCat   = c^.cEntCat
         , _mcEntities = c^.cEntities
         }

writeTileState ∷ (MonadThrow μ, PrimMonad μ)
               ⇒ TileOffset
               → TileState τ
               → MutableChunk (PrimState μ)
               → μ ()
writeTileState off@(TileOffset { x, y, .. }) ts mc
  = assertValidOffset off $
    let x'  = fromIntegral x ∷ Int
        y'  = fromIntegral y ∷ Int
        z'  = fromIntegral z ∷ Int
    in
      do its ← toIndexed (mc^.mcTilePal) ts
         GMV.write (mc^.mcTiles) (y'⋅chunkHeight⋅chunkSize + x'⋅chunkHeight + z') its

writeClimate ∷ PrimMonad μ ⇒ TileOffset → Climate → MutableChunk (PrimState μ) → μ ()
writeClimate off@(TileOffset { x, y, .. }) cli mc
  = assertValidOffset off $
    let x' = fromIntegral x ∷ Int
        y' = fromIntegral y ∷ Int
    in
      GMV.write (mc^.mcClimates) (y'⋅chunkSize + x') cli

#if defined(DEBUG)
writeRiver ∷ PrimMonad μ ⇒ TileOffset → Float → MutableChunk (PrimState μ) → μ ()
writeRiver off@(TileOffset { x, y, .. }) river mc
  = assertValidOffset off $
    let x' = fromIntegral x ∷ Int
        y' = fromIntegral y ∷ Int
    in
      GMV.write (mc^.mcRivers) (y'⋅chunkSize + x') river
#endif

writeBiome ∷ (Biome β, MonadThrow μ, PrimMonad μ)
           ⇒ TileOffset
           → β
           → MutableChunk (PrimState μ)
           → μ ()
writeBiome off@(TileOffset { x, y, .. }) b mc
  = assertValidOffset off $
    let x' = fromIntegral x ∷ Int
        y' = fromIntegral y ∷ Int
    in
      do bi ← BPal.indexOf (biomeID b) (mc^.mcBiomePal)
         GMV.write (mc^.mcBiomes) (y'⋅chunkSize + x') bi
