{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Biome.Palette
  ( -- * Types
    BiomePalette
  , BiomeIndex

    -- * Constructing palettes
  , empty
  , fromRegistry

    -- * Manipulating palettes
  , insert

    -- * Querying palettes
  , indexOf
  , idOf

    -- * Exceptions
  , UnknownBiomeIndexException(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.MonoTraversable (ofoldl')
import Data.Word (Word16)
import Game.AcidRain.World.Biome (BiomeID, biomeID)
import Game.AcidRain.World.Biome.Registry
  ( BiomeRegistry, UnknownBiomeIDException(..) )


-- | Numerical index for biomes. We use an unsigned 16-bits word as an
-- index so we can have at most @2^16@ tiles.
type BiomeIndex = Word16

-- | Biome palette is a bidirectional map between 'BiomeID' and
-- numerical biome index. It is used for compressing chunk data both in
-- memory and on disk. The same palette is shared between all the
-- chunks in a world. A palette is constructed while loading a world,
-- and becomes immutable afterwards.
--
-- A palette saved on disk may contain biomes that no longer
-- exist. When we load a palette and find some biomes are missing, we
-- display a warning about that. And when we load a chunk which
-- contains missing biomes, we replace them with acid-rain:plains or
-- something equivalently convincing.
--
-- This type is not even a 'Monoid' because merging two palettes can
-- fail due to duplicating biome IDs or indices.
data BiomePalette =
  BiomePalette
  { indexOf' ∷ !(HashMap BiomeID BiomeIndex)
  , idOf'    ∷ !(Map BiomeIndex BiomeID) -- We can't use IntMap because it's not Int.
  } deriving (Show)

-- | Create an empty palette.
empty ∷ BiomePalette
empty = BiomePalette HM.empty M.empty

-- | Construct a palette out of a biome registry by assigning a unique
-- index for each registered biome. This is only useful while creating
-- a fresh new world, as indices must match with any existing chunks.
fromRegistry ∷ BiomeRegistry → BiomePalette
fromRegistry = ofoldl' (\p b → insert (biomeID b) p) empty

-- | Insert a biome ID to the palette and assign a new index for
-- it. Inserting the same ID twice is not an error. It will just be
-- ignored.
insert ∷ BiomeID → BiomePalette → BiomePalette
insert bid p
  = case HM.member bid $ indexOf' p of
      True  → p
      False →
        let idx = case M.lookupMax $ idOf' p of
                    Just (maxIdx, _) → maxIdx + 1
                    Nothing          → 0
        in
          BiomePalette
          { indexOf' = HM.insert bid idx $ indexOf' p
          , idOf'    = M.insert idx bid $ idOf' p
          }

-- | Find a biome index by a biome ID.
indexOf ∷ MonadThrow μ ⇒ BiomeID → BiomePalette → μ BiomeIndex
indexOf bid p
  = case HM.lookup bid $ indexOf' p of
      Just idx → return idx
      Nothing  → throwM $ UnknownBiomeIDException bid

-- | Find a biome ID by a biome index.
idOf ∷ MonadThrow μ ⇒ BiomeIndex → BiomePalette → μ BiomeID
idOf idx p
  = case M.lookup idx $ idOf' p of
      Just bid → return bid
      Nothing  → throwM $ UnknownBiomeIndexException idx

-- | An exception to be thrown when there was no biome ID having the
-- given index.
data UnknownBiomeIndexException = UnknownBiomeIndexException BiomeIndex
  deriving Show

instance Exception UnknownBiomeIndexException
