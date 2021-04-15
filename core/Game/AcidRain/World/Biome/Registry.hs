{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Biome.Registry
  ( -- * The registry type
    BiomeRegistry

    -- * Constructing registries
  , empty

    -- * Manipulating registries
  , register

    -- * Querying registries
  , lookup
  , get

    -- * Exceptions
  , ConflictingBiomeIDException(..)
  , UnknownBiomeIDException(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.MonoTraversable
  ( Element, MonoFunctor, MonoFoldable, MonoTraversable, GrowingAppend
  , otraverse )
import Data.Poly.Strict (Poly)
import Game.AcidRain.World.Biome (Biome(..), BiomeID)
import Prelude hiding (lookup)

-- | The biome registry is a data structure that contains immutable
-- 'Biome' objects. It is constructed while loading a world, and
-- becomes immutable afterwards.
newtype BiomeRegistry = BiomeRegistry (HashMap BiomeID (Poly Biome))
  deriving ( Show, MonoFunctor, MonoFoldable, GrowingAppend, Semigroup
           , Monoid )

-- GeneralisedNewtypeDeriving can't derive MonoTraversable for us due
-- to a limitation of the compiler. So we do it manually.
instance MonoTraversable BiomeRegistry where
  otraverse f (BiomeRegistry reg)
    = BiomeRegistry <$> traverse f reg

type instance Element BiomeRegistry = Poly Biome

-- | Create an empty registry.
empty ∷ BiomeRegistry
empty = BiomeRegistry HM.empty

-- | Register a biome to the registry. Throws if it's already been
-- registered.
register ∷ (Biome β, MonadThrow μ) ⇒ β → BiomeRegistry → μ BiomeRegistry
register biome (BiomeRegistry reg)
  = let bid = biomeID biome
    in
      case HM.member bid reg of
        True  → throwM $ ConflictingBiomeIDException bid
        False → return $ BiomeRegistry $ HM.insert bid (upcastBiome biome) reg

-- | Lookup a biome by its ID. Return 'Nothing' if no biomes matching
-- with the given ID has been registered.
lookup ∷ BiomeID → BiomeRegistry → Maybe (Poly Biome)
lookup bid (BiomeRegistry reg)
  = HM.lookup bid reg

-- | Get a biome by its ID. Throws if it doesn't exist.
get ∷ MonadThrow μ ⇒ BiomeID → BiomeRegistry → μ (Poly Biome)
get bid reg
  = case lookup bid reg of
      Just biome → return biome
      Nothing    → throwM $ UnknownBiomeIDException bid

-- | An exception to be thrown when two biomes with the same ID is
-- being registered.
data ConflictingBiomeIDException = ConflictingBiomeIDException !BiomeID
  deriving Show

instance Exception ConflictingBiomeIDException

-- | An exception to be thrown when there was no biome having the given
-- ID.
data UnknownBiomeIDException = UnknownBiomeIDException !BiomeID
  deriving Show

instance Exception UnknownBiomeIDException
