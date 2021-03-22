{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo
  ( WorldInfo
  , worldInfo

  , simplexInstance
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.Reader.Lazy (Reader, reader)
import Data.List (unfoldr)
import Game.AcidRain.World (WorldSeed)
import Numeric.Noise.OpenSimplex (SimplexGen, mkSimplexGen)


-- | Per-world cache and configurations for chunk generation.
data WorldInfo
  = WorldInfo
    { -- | An infinite list of simplex noise generators of varying
      -- seeds. The starting seed is that of the world seed.
      wiSimplices ∷ [SimplexGen]
    }

-- | Construct a 'WorldInfo'.
worldInfo ∷ WorldSeed → WorldInfo
worldInfo seed
  = WorldInfo
    { wiSimplices = unfoldr (\x → Just (mkSimplexGen x, x + 1)) seed
    }

-- | Get the n-th cached instance of SimplexGen for use in chunk
-- generator. Try to keep the @n@ as small as possible, or it will eat
-- up all the memory.
simplexInstance ∷ Member (Reader WorldInfo) r ⇒ Int → Eff r SimplexGen
simplexInstance n
  = (!! n) <$> reader wiSimplices
