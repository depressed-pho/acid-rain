{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk
  ( TileOffset(..)
  , Chunk
  , chunkSize
  , chunkHeight
  , new
  , tileStateAt
  ) where

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow)
import Data.Convertible.Base (Convertible(..))
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import Data.Word (Word8)
import Game.AcidRain.World.Chunk.Palette (ChunkPalette, TileIndex, indexOf, idOf)
import Game.AcidRain.World.Position (WorldPos(..))
import Game.AcidRain.World.Tile (Tile(..), TileState(..), TileStateValue)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Prelude.Unicode ((⋅))


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
    { itsIndex ∷ !TileIndex
    , itsValue ∷ !TileStateValue
    } deriving (Show, Eq)

toIndexed ∷ MonadThrow μ ⇒ ChunkPalette → TileState → μ IndexedTileState
toIndexed palette ts
  = do idx ← indexOf (tileID $ tsTile ts) palette
       return $ IndexedTileState
         { itsIndex = idx
         , itsValue = tsValue ts
         }

-- | This is an offset to a tile in a chunk, convertible from 'WorldPos'.
data TileOffset
  = TileOffset
    { x ∷ !Word8
    , y ∷ !Word8
    , z ∷ !Word8
    } deriving (Show)

instance Convertible WorldPos TileOffset where
  safeConvert (WorldPos { x, y, z })
    = Right $ TileOffset
      { x = fromIntegral $ x `mod` chunkSize
      , y = fromIntegral $ y `mod` chunkSize
      , z = fromIntegral $ z `mod` chunkHeight
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
    { registry ∷ !TileRegistry
    , palette  ∷ !ChunkPalette
    , tiles    ∷ !(UV.Vector IndexedTileState)
    }

-- | Create a chunk filled with a single specific tile which is
-- usually @acid-rain:air@.
new ∷ MonadThrow μ ⇒ TileRegistry → ChunkPalette → TileState → μ Chunk
new registry palette fill
  = do its ← toIndexed palette fill
       return $ Chunk
         { registry
         , palette
         , tiles = GV.replicate (chunkSize ⋅ chunkSize ⋅ chunkHeight) its
         }

-- | Get a tile state at a given offset in a tile.
tileStateAt ∷ MonadThrow μ ⇒ TileOffset → Chunk → μ TileState
tileStateAt (TileOffset { x, y, z }) chunk
  = assert (x < chunkSize) $
    assert (y < chunkSize) $
    assert (z < chunkHeight) $
    let x'  = fromIntegral x ∷ Int
        y'  = fromIntegral y ∷ Int
        z'  = fromIntegral z ∷ Int
        its = tiles chunk GV.! (z' ⋅ chunkHeight ⋅ chunkSize + y' ⋅ chunkSize + x')
    in
      do tid  ← idOf (itsIndex its) (palette chunk)
         tile ← TR.get tid (registry chunk)
         return $ TileState
           { tsTile  = tile
           , tsValue = itsValue its
           }
