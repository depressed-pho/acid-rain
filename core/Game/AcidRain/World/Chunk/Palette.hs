{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk.Palette
  ( TileIndex
  , ChunkPalette
  , empty
  , insert
  , indexOf
  , idOf
  , UnknownTileIndexException(..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import Game.AcidRain.World.Tile (TileID)
import Game.AcidRain.World.Tile.Registry (UnknownTileIDException(..))


type TileIndex = Word32

-- | A chunk palette is a bidirectional map between tile ID from/to
-- numerical tile index. It is used for compressing chunk data both in
-- memory and on disk. The same palette is shared between all the
-- chunks in a world.
--
-- A palette saved on disk may contain tiles that no longer
-- exist. When we load a palette and find some tiles are missing, we
-- display a warning about that. And when we load a chunk which
-- contains missing tiles, we replace them with acid-rain:air or
-- something equivalently convincing.
--
-- A palette is constructed while loading a world, and becomes
-- immutable afterwards.
data ChunkPalette = ChunkPalette
  { indexOf' ∷ !(HashMap TileID TileIndex)
  , idOf'    ∷ !(Map TileIndex TileID) -- We can't use IntMap because it's not Int.
  } deriving (Show)

-- | Create an empty palette.
empty ∷ ChunkPalette
empty = ChunkPalette HM.empty M.empty

-- | Insert a tile ID to the palette. Inserting the same ID twice is
-- not an error. It will just be ignored.
insert ∷ TileID → ChunkPalette → ChunkPalette
insert tid p
  = case HM.member tid $ indexOf' p of
      True  → p
      False →
        let idx = case M.lookupMax $ idOf' p of
                    Just (maxIdx, _) → maxIdx + 1
                    Nothing          → 0
        in
          ChunkPalette
          { indexOf' = HM.insert tid idx $ indexOf' p
          , idOf'    = M.insert idx tid $ idOf' p
          }

-- | Find a tile index by a tile ID.
indexOf ∷ MonadThrow m ⇒ TileID → ChunkPalette → m TileIndex
indexOf tid p
  = case HM.lookup tid $ indexOf' p of
      Just idx → return idx
      Nothing  → throwM $ UnknownTileIDException tid

-- | Find a tile ID by tile index.
idOf ∷ MonadThrow m ⇒ TileIndex → ChunkPalette → m TileID
idOf idx p
  = case M.lookup idx $ idOf' p of
      Just tid → return tid
      Nothing  → throwM $ UnknownTileIndexException idx

-- | An exception to be thrown when there was no tile ID having the
-- given index.
data UnknownTileIndexException = UnknownTileIndexException TileIndex
  deriving Show

instance Exception UnknownTileIndexException
