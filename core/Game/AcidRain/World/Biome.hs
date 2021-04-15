{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Biome
  ( Biome(..)
  , BiomeID
  ) where

import Data.Poly.Strict (Poly(..))
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
  upcastBiome ∷ β → Poly Biome
  upcastBiome = Poly
  -- | Recover the type of the biome.
  downcastBiome ∷ Poly Biome → Maybe β
  downcastBiome (Poly b) = cast b
  -- | Get the biome ID such as @acid-rain:ocean@.
  biomeID ∷ β → BiomeID

instance Show (Poly Biome) where
  showsPrec d (Poly b) = showsPrec d b

instance Biome (Poly Biome) where
  upcastBiome = id
  downcastBiome = Just
  biomeID (Poly b) = biomeID b
