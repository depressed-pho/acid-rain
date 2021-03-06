{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo
  ( WorldInfo(..)
  , worldInfo

  , simplexInstance
  , voronoiInstance
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.Reader.Lazy (Reader, reader)
import Data.List (unfoldr)
import Game.AcidRain.World (WorldSeed)
import Numeric.Natural (Natural)
import Numeric.Noise.OpenSimplex (SimplexGen, mkSimplexGen)
import Numeric.Noise.Voronoi (VoronoiGen, mkVoronoiGen)


-- | Per-world cache and configurations for chunk generation.
data WorldInfo
  = WorldInfo
    { -- | An infinite list of simplex noise generators of varying
      -- seeds. The starting seed is that of the world seed.
      wiSimplices ∷ [SimplexGen]
      -- | An infinite list of Voronoi noise generators of varying
      -- seeds. The starting seed is that of the world seed.
    , wiVoronois ∷ [VoronoiGen Double]
      -- | The scale for the low-frequency height map. The lower the
      -- scale is, the smoother the heigt map is. Make it too high and
      -- oceans generate like a cluster of lakes.
    , wiLoFreqHeightMapScale ∷ !Double
      -- | The scale for the high-frequency height map. The lower the
      -- scaleis, the smoother the heigt map is.
    , wiHiFreqHeightMapScale ∷ !Double
      -- | The amplitude for the high-frequency height map in [0, 1].
    , wiHiFreqHeightMapAmp ∷ !Double
      -- | Ocean / land ratio in [0, 1].
    , wiOceanLandRatio ∷ !Double
      -- | Mountain and valley enhancement factor. Use zero to apply
      -- no enhancements. Higher values turn the height exponentially
      -- close to zero.
    , wiMountainExp ∷ !Double
      -- | The size of biomes.
    , wiBiomeSize ∷ !Double
      -- | The large bend size of rivers.
    , wiLargeBendSize ∷ !Double
      -- | The small bend size of rivers.
    , wiSmallBendSize ∷ !Double
      -- | The river separation factor.
    , wiRiverSeparation ∷ !Double
      -- | The river valley level.
    , wiRiverValleyLevel ∷ !Double
    }

-- | Construct a 'WorldInfo'.
worldInfo ∷ WorldSeed → WorldInfo
worldInfo seed
  = WorldInfo
    { wiSimplices            = unfoldr (\x → Just (mkSimplexGen x, x + 1)) seed
    , wiVoronois             = unfoldr (\x → Just (mkVoronoiGen x, x + 1)) seed
    , wiLoFreqHeightMapScale = 1/20
    , wiHiFreqHeightMapScale = 3
    , wiHiFreqHeightMapAmp   = 0.3
    , wiOceanLandRatio       = 0.4
    , wiMountainExp          = 0.8
    , wiBiomeSize            = 350
    , wiLargeBendSize        = 140
    , wiSmallBendSize        = 30
    , wiRiverSeparation      = 975
    , wiRiverValleyLevel     = 140/450
    }

-- | Get the n-th cached instance of SimplexGen for use in chunk
-- generator. Try to keep the @n@ as small as possible, or it will eat
-- up all the memory.
simplexInstance ∷ Member (Reader WorldInfo) r ⇒ Natural → Eff r SimplexGen
simplexInstance n
  = (!! fromEnum n) <$> reader wiSimplices

-- | Get the n-th cached instance of VoronoiGen for use in chunk
-- generator. Try to keep the @n@ as small as possible, or it will eat
-- up all the memory.
voronoiInstance ∷ Member (Reader WorldInfo) r ⇒ Natural → Eff r (VoronoiGen Double)
voronoiInstance n
  = (!! fromEnum n) <$> reader wiVoronois
