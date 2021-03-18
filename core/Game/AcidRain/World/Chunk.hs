{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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
  , entityAt
  ) where

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow)
import Data.Convertible.Base (Convertible(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import Data.Word (Word8)
import GHC.Generics (Generic)
import Game.AcidRain.World.Chunk.Palette (TilePalette, TileIndex, indexOf, idOf)
import Game.AcidRain.World.Entity (EntityType(..), Entity(..), SomeEntity)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue, (∈))
import Game.AcidRain.World.Position (WorldPos(..), wpX, wpY, wpZ)
import Game.AcidRain.World.Tile (Tile(..), TileState(..), TileStateValue, SomeTileState)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Lens.Micro ((&), (^.), (%~))
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((∘), (⋅))


-- | Unlike Minecraft our chunks are only two blocks tall so we can
-- use a larger chunk size than 16*16.
chunkSize ∷ Integral i ⇒ i
chunkSize = 32

chunkHeight ∷ Integral i ⇒ i
chunkHeight = 2

-- | This is a variant of 'TileState' which has 'TileIndex' instead of
-- Tile itself. This representation is used both on disk and in memory
-- chunk data to save space.
data IndexedTileState
  = IndexedTileState
    { itsIndex ∷ {-# UNPACK #-} !TileIndex
    , itsValue ∷ {-# UNPACK #-} !TileStateValue
    } deriving (Show, Eq)

toIndexed ∷ MonadThrow μ ⇒ TilePalette → TileState τ → μ IndexedTileState
toIndexed palette (TileState { tsTile, tsValue })
  = do idx ← indexOf (tileID tsTile) palette
       return $ IndexedTileState
         { itsIndex = idx
         , itsValue = tsValue
         }

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

-- Assert that the given offset is valid.
assertValidOffset ∷ TileOffset → α → α
assertValidOffset (TileOffset { x, y, z })
  = assert (x < chunkSize) ∘
    assert (y < chunkSize) ∘
    assert (z < chunkHeight)

-- Assert that the given entity is in the catalogue.
assertValidEntity ∷ Entity ε ⇒ ε → Chunk → α → α
assertValidEntity e c
  = assert (entityTypeID (entityType e) ∈ c^.cEntCat)

-- | Create a chunk filled with a single specific tile which is
-- usually @acid-rain:air@.
new ∷ MonadThrow μ
    ⇒ TileRegistry
    → TilePalette
    → EntityCatalogue
    → TileState τ
    → μ Chunk
new tReg tPal eCat fill
  = do its ← toIndexed tPal fill
       return $ Chunk
         { _cTileReg  = tReg
         , _cTilePal  = tPal
         , _cTiles    = GV.replicate (chunkSize ⋅ chunkSize ⋅ chunkHeight) its
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

-- | Get a tile state at a given offset in a chunk.
tileStateAt ∷ MonadThrow μ ⇒ TileOffset → Chunk → μ SomeTileState
tileStateAt off@(TileOffset { x, y, z }) c
  = assertValidOffset off $
    let x'  = fromIntegral x ∷ Int
        y'  = fromIntegral y ∷ Int
        z'  = fromIntegral z ∷ Int
        its = (c^.cTiles) GV.! (z' ⋅ chunkHeight ⋅ chunkSize + y' ⋅ chunkSize + x')
    in
      do tid  ← idOf (itsIndex its) (c^.cTilePal)
         tile ← TR.get tid (c^.cTileReg)
         return $ TileState
           { tsTile  = tile
           , tsValue = itsValue its
           }

-- | Lookup an entity possibly located at a given offset in a chunk.
entityAt ∷ TileOffset → Chunk → Maybe SomeEntity
entityAt off c
  = assertValidOffset off $
    HM.lookup off (c^.cEntities)
