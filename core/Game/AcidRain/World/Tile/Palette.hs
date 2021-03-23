{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile.Palette
  ( -- * Types
    TilePalette
  , TileIndex

    -- * Constructing palettes
  , empty
  , fromRegistry

    -- * Manipulating palettes
  , insert

    -- * Querying palettes
  , indexOf
  , idOf

    -- * Exceptions
  , UnknownTileIndexException(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.MonoTraversable (ofoldl')
import Data.Word (Word32)
import Game.AcidRain.World.Tile (TileID, tileID)
import Game.AcidRain.World.Tile.Registry
  ( TileRegistry, UnknownTileIDException(..) )


-- | Numerical index for tiles. We use an unsigned 32-bits word as an
-- index so we can have at most @2^32@ tiles.
--
-- While using 32 bits might seem overkill, a tile in a chunk is a
-- pair of 'TileIndex' and 'TileStateValue' so even if we were to use
-- only 16 bits for indices we would be consuming 48 bits per tile
-- (because 'TileStateValue' is also a 'Word32'). Dunno if unboxed
-- vectors pad elements to align them but if they didn't that would
-- lead to unaligned reads and writes. So we are probably not wasting
-- any space.
type TileIndex = Word32

-- | Tile palette is a bidirectional map between 'TileID' and
-- numerical tile index. It is used for compressing chunk data both in
-- memory and on disk. The same palette is shared between all the
-- chunks in a world. A palette is constructed while loading a world,
-- and becomes immutable afterwards.
--
-- A palette saved on disk may contain tiles that no longer
-- exist. When we load a palette and find some tiles are missing, we
-- display a warning about that. And when we load a chunk which
-- contains missing tiles, we replace them with acid-rain:air or
-- something equivalently convincing.
--
-- This type is not even a 'Monoid' because merging two palettes can
-- fail due to duplicating tile IDs or indices.
data TilePalette =
  TilePalette
  { indexOf' ∷ !(HashMap TileID TileIndex)
  , idOf'    ∷ !(Map TileIndex TileID) -- We can't use IntMap because it's not Int.
  } deriving (Show)

-- | Create an empty palette.
empty ∷ TilePalette
empty = TilePalette HM.empty M.empty

-- | Construct a palette out of a tile registry by assigning a unique
-- index for each registered tile. This is only useful while creating
-- a fresh new world, as indices must match with any existing chunks.
fromRegistry ∷ TileRegistry → TilePalette
fromRegistry = ofoldl' (\p t → insert (tileID t) p) empty

-- | Insert a tile ID to the palette and assign a new index for
-- it. Inserting the same ID twice is not an error. It will just be
-- ignored.
insert ∷ TileID → TilePalette → TilePalette
insert tid p
  = case HM.member tid $ indexOf' p of
      True  → p
      False →
        let idx = case M.lookupMax $ idOf' p of
                    Just (maxIdx, _) → maxIdx + 1
                    Nothing          → 0
        in
          TilePalette
          { indexOf' = HM.insert tid idx $ indexOf' p
          , idOf'    = M.insert idx tid $ idOf' p
          }

-- | Find a tile index by a tile ID.
indexOf ∷ MonadThrow μ ⇒ TileID → TilePalette → μ TileIndex
indexOf tid p
  = case HM.lookup tid $ indexOf' p of
      Just idx → return idx
      Nothing  → throwM $ UnknownTileIDException tid

-- | Find a tile ID by a tile index.
idOf ∷ MonadThrow μ ⇒ TileIndex → TilePalette → μ TileID
idOf idx p
  = case M.lookup idx $ idOf' p of
      Just tid → return tid
      Nothing  → throwM $ UnknownTileIndexException idx

-- | An exception to be thrown when there was no tile ID having the
-- given index.
data UnknownTileIndexException = UnknownTileIndexException TileIndex
  deriving Show

instance Exception UnknownTileIndexException
