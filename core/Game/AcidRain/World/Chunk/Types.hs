{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk.Types
  ( TileOffset(..)
  , chunkSize
  , chunkHeight

  , IndexedTileState(..)
  , Chunk(..), cTileReg, cTilePal, cTiles, cClimates, cEntCat, cEntities
  , MutableChunk(..), mcTileReg, mcTilePal, mcClimates, mcTiles, mcEntCat, mcEntities
  , freezeChunk
  , thawChunk
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Convertible.Base (Convertible(..))
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import Data.Word (Word8)
import GHC.Generics (Generic)
import Game.AcidRain.World.Chunk.Palette (TilePalette, TileIndex)
import Game.AcidRain.World.Climate (Climate)
import Game.AcidRain.World.Entity (SomeEntity)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue)
import Game.AcidRain.World.Position (WorldPos(..), wpX, wpY, wpZ)
import Game.AcidRain.World.Tile (TileStateValue)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.))
import Prelude.Unicode ((∘))


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

-- | The tile state vector.
newtype instance UV.MVector σ IndexedTileState = MV_ITS (UV.MVector σ (TileIndex, TileStateValue))
newtype instance UV.Vector    IndexedTileState = V_ITS  (UV.Vector    (TileIndex, TileStateValue))

instance GMV.MVector UV.MVector IndexedTileState where
  basicLength (MV_ITS v) = GMV.basicLength v
  basicUnsafeSlice s l (MV_ITS v) = MV_ITS $ GMV.basicUnsafeSlice s l v
  basicOverlaps (MV_ITS v) (MV_ITS v') = GMV.basicOverlaps v v'
  basicUnsafeNew l = MV_ITS <$> GMV.basicUnsafeNew l
  basicInitialize (MV_ITS v) = GMV.basicInitialize v
  basicUnsafeRead (MV_ITS v) i
    = do (idx, val) ← GMV.basicUnsafeRead v i
         return $ IndexedTileState idx val
  basicUnsafeWrite (MV_ITS v) i its
    = GMV.basicUnsafeWrite v i (itsIndex its, itsValue its)

instance GV.Vector UV.Vector IndexedTileState where
  basicUnsafeFreeze (MV_ITS v) = V_ITS <$> GV.basicUnsafeFreeze v
  basicUnsafeThaw (V_ITS v) = MV_ITS <$> GV.basicUnsafeThaw v
  basicLength (V_ITS v) = GV.basicLength v
  basicUnsafeSlice s l (V_ITS v) = V_ITS $ GV.basicUnsafeSlice s l v
  basicUnsafeIndexM (V_ITS v) i
    = do (idx, val) ← GV.basicUnsafeIndexM v i
         return $ IndexedTileState idx val

data Chunk
  = Chunk
    { _cTileReg  ∷ !TileRegistry
    , _cTilePal  ∷ !TilePalette
    , _cTiles    ∷ !(UV.Vector IndexedTileState)
    , _cClimates ∷ !(UV.Vector Climate)
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
    , _mcEntCat   ∷ !EntityCatalogue
    , _mcEntities ∷ !(HashMap TileOffset SomeEntity)
    }

makeLenses ''MutableChunk

freezeChunk ∷ PrimMonad μ ⇒ MutableChunk (PrimState μ) → μ Chunk
freezeChunk mc
  = do tiles    ← GV.freeze $ mc^.mcTiles
       climates ← GV.freeze $ mc^.mcClimates
       return Chunk
         { _cTileReg  = mc^.mcTileReg
         , _cTilePal  = mc^.mcTilePal
         , _cTiles    = tiles
         , _cClimates = climates
         , _cEntCat   = mc^.mcEntCat
         , _cEntities = mc^.mcEntities
         }

thawChunk ∷ PrimMonad μ ⇒ Chunk → μ (MutableChunk (PrimState μ))
thawChunk c
  = do tiles    ← GV.thaw $ c^.cTiles
       climates ← GV.thaw $ c^.cClimates
       return MutableChunk
         { _mcTileReg  = c^.cTileReg
         , _mcTilePal  = c^.cTilePal
         , _mcTiles    = tiles
         , _mcClimates = climates
         , _mcEntCat   = c^.cEntCat
         , _mcEntities = c^.cEntities
         }
