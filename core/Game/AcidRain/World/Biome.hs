{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Biome
  ( Biome(..)
  , BiomeID
  , SomeBiome(..)
  ) where

import Data.Text (Text)
import Data.Typeable (Typeable, cast)


type BiomeID = Text

-- | An type in this class defines a biome in the game. A biome at a
-- specific location is represented solely as a 'Biome' and the values
-- of 'Biome' types are shared across the entire world. For this
-- reason, a type in 'Biome' typically has no values, and instead
-- implements the class on 'Data.Proxy.Proxy' like:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- data Ocean
-- instance 'Tile' ('Data.Proxy.Proxy' Ocean) where
--   biomeID _ = "acid-rain:ocean"
--   ..
-- @
class (Show β, Typeable β) ⇒ Biome β where
  -- | Erase the type of the biome.
  upcastBiome ∷ β → SomeBiome
  upcastBiome = SomeBiome
  -- | Recover the type of the biome.
  downcastBiome ∷ SomeBiome → Maybe β
  downcastBiome (SomeBiome b) = cast b
  -- | Get the biome ID such as @acid-rain:ocean@.
  biomeID ∷ β → BiomeID

-- | A type-erased 'Biome'.
data SomeBiome = ∀β. Biome β ⇒ SomeBiome !β

instance Show SomeBiome where
  showsPrec d (SomeBiome b) = showsPrec d b

instance Biome SomeBiome where
  upcastBiome = id
  downcastBiome = Just
  biomeID (SomeBiome b) = biomeID b
