{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen.PointAttrs
  ( PointAttrs(..)

    -- * Computing point attributes
  , pointAttrs
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.Reader.Lazy (Reader, ask)
import Data.Poly.Strict (Poly)
import Game.AcidRain.Module.Builtin.Biomes
  ( BiomeChunkGen, BiomeChooser, chooseBiome )
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo
  ( WorldInfo(..), simplexInstance, voronoiInstance )
import Game.AcidRain.World.Climate (Climate(..))
import Game.AcidRain.World.Position (WorldPos, wpX, wpY)
import Numeric.Noise.FBM (fBm)
import Numeric.Noise.OpenSimplex (SimplexGen, Scalar(..), simplex2D, scalar, diskX, diskY)
import Numeric.Noise.Voronoi (voronoi2D, interiorValue)
import Lens.Micro ((^.))
import Prelude.Unicode ((⋅), (≥), (≤))


-- | Point attributes describe a set of attributes determined for each
-- @(x, y)@ coordinates in a world.
data PointAttrs
  = PointAttrs
    { -- | The height at this point in @[-1, 1]@. It is generated with
      -- a domain-warped fBm (see
      -- <https://github.com/dandrino/terrain-erosion-3-ways>) and is
      -- then further affected by a river strength which is computed
      -- using a jittered Voronoi noise.
      paHeight ∷ !Double
      -- | River strength in @[-1, 0]@.
    , paRiver ∷ !Double
      -- | The climate at this point. The altitude is calculated based
      -- on the height. Temperature and humidity are generated with a
      -- simplex noise but they of course are affected by the
      -- altitude.
    , paClimate ∷ !Climate
      -- | Per-biome chunk generator chosen for this point.
    , paBiome   ∷ !(Poly BiomeChunkGen)
    } deriving (Show)

-- | Compute 'PointAttrs' for a given @(x, y)@ coordinates. The @z@
-- coordinate is ignored.
pointAttrs ∷ Member (Reader WorldInfo) r ⇒ BiomeChooser → WorldPos → Eff r PointAttrs
pointAttrs bc pos
  = do r ← riverStrength pos
       h ← height r pos
       c ← climate h pos
       return PointAttrs
         { paHeight  = h
         , paRiver   = r
         , paClimate = c
         , paBiome   = chooseBiome h r c bc
         }

-- | Compute the final height for a given (x, y) coordinates. For the
-- base height we compute @fBm(p + fBm(p + fBm(p)))@ for the point
-- @p@.
height ∷ ∀r. Member (Reader WorldInfo) r ⇒ Double → WorldPos → Eff r Double
height river pos
  = do lfh ← lfHeight
       hfh ← hfHeight
       let baseHeight = clamp (lfh + hfh) (-1) 1

       -- Now we obtained a height in [-1, 1] with a uniform
       -- distribution, which means 50% of the world is sunk in
       -- water. For the beter game play we change this distribution
       -- by remapping the range to [lo, hi] for some (lo, hi) ∈ [-1,
       -- 1]. The resulting height is still in [-1, 1].
       wi      ← ask
       let ratio = wiOceanLandRatio wi
           -- ratio   0 → [ 0, 1]
           -- ratio 0.5 → [-1, 1]
           -- ratio   1 → [-1, 0]
           lo = mix 0 (-1)     ((clamp ratio 0   0.5)⋅2)
           hi = mix 0   1  (1-(((clamp ratio 0.5 1  )⋅2)-1))
           biasedHeight
             = mix lo hi ((baseHeight+1)/2)

       -- Enhance mountains and valleys by multiplying it by its
       -- absolute value on the power of some constant.
       let enhancedHeight
             = biasedHeight⋅(abs biasedHeight)**(wiMountainExp wi)

       -- Then apply a river strength unless the height is already
       -- under the sea level.
       return $ riverize enhancedHeight river
  where
    lfHeight ∷ Eff r Double
    lfHeight
      = do wi      ← ask
           let pt0 = ( fromIntegral (pos^.wpX) ⋅ wiLoFreqHeightMapScale wi
                     , fromIntegral (pos^.wpY) ⋅ wiLoFreqHeightMapScale wi )

           noiseX  ← simplexInstance 0
           noiseY  ← simplexInstance 1
           let pt1 = ( evalFBm noiseX pt0
                     , evalFBm noiseY pt0 )

           noiseX' ← simplexInstance 2
           noiseY' ← simplexInstance 3
           let pt2 = ( evalFBm noiseX' (offset pt0 (scale warp pt1))
                     , evalFBm noiseY' (offset pt0 (scale warp pt1)) )

           noiseF  ← simplexInstance 4
           return $ evalFBm noiseF (offset pt0 (scale warp pt2))

    hfHeight ∷ Eff r Double
    hfHeight
      = do wi ← ask
           let pt = ( fromIntegral (pos^.wpX) ⋅ wiHiFreqHeightMapScale wi
                    , fromIntegral (pos^.wpY) ⋅ wiHiFreqHeightMapScale wi )
           noiseF ← simplexInstance 5
           return $ evalFBm noiseF pt ⋅ wiHiFreqHeightMapAmp wi

    evalFBm ∷ SimplexGen → (Double, Double) → Double
    evalFBm gen pt
      = fBm scale (noise gen) oct 1 0.5 freq0 2 pt

    offset ∷ (Double, Double) → (Double, Double) → (Double, Double)
    offset (x0, y0) (x1, y1) = (x0+x1, y0+y1)

    scale ∷ Double → (Double, Double) → (Double, Double)
    scale s (x, y) = (s⋅x, s⋅y)

    noise ∷ SimplexGen → (Double, Double) → Double
    noise gen (x, y) = (simplex2D gen x y)^.scalar

    -- Domain warp factor
    warp ∷ Double
    warp = 4

    -- The number of octaves
    oct ∷ Int
    oct = 6

    -- The initial frequency
    freq0 ∷ Double
    freq0 = 1 / 300

-- | Corrode the terrain based on the 'riverStrength'.
riverize ∷ Double → Double → Double
riverize baseHeight river
  | baseHeight ≤ 0
    = baseHeight -- Already sunk in water.
  | otherwise
    = let adjustment = baseHeight * 6.4 + 0.6
          river'     = bayesianAdjustment (river + 1) adjustment
      in
        -- Adjustment to make riverbanks more varied depending on
        -- height. The lower the height is, the wider the river should
        -- be.
        baseHeight ⋅ river'

-- | Get the river strengh at a given world position. River strength
-- is a scalar value from @-1.0@ to @0.0@ where @-1.0@ means
-- "seriously deep river" and @0.0@ means "not a river at all". The Z
-- coordinate of the position is ignored. This algorithm is based on
-- Realistic-Terrain-Generation.
riverStrength ∷ Member (Reader WorldInfo) r ⇒ WorldPos → Eff r Double
riverStrength pos
  = do let pX0 = fromIntegral (pos^.wpX)
           pY0 = fromIntegral (pos^.wpY)
       bendLarge ← (\s → simplex2D s (pX0 / bendScaleL) (pY0 / bendScaleL)) <$> simplexInstance 1
       bendSmall ← (\s → simplex2D s (pX0 / bendScaleS) (pY0 / bendScaleS)) <$> simplexInstance 2
       wi ← ask
       let pX = ( pX0
                  + (bendLarge^.diskX) ⋅ (wiLargeBendSize wi)
                  + (bendSmall^.diskX) ⋅ (wiLargeBendSize wi) )
                / (wiRiverSeparation wi)
           pY = ( pY0
                  + (bendLarge^.diskY) ⋅ (wiLargeBendSize wi)
                  + (bendSmall^.diskY) ⋅ (wiLargeBendSize wi) )
                / (wiRiverSeparation wi)
       -- Generate a cellular noise based on the jittered coordinates.
       -- The output is a curved function of relative distance from
       -- the center, so adjust to make it flatter.
       voronoi ← (\v → voronoi2D v pX pY) <$> voronoiInstance 0
       let riverFactor0 = interiorValue voronoi
           riverFactor  = bayesianAdjustment riverFactor0 0.5
       if riverFactor > wiRiverValleyLevel wi
         then return 0 -- No river effect
         else return $ riverFactor / wiRiverValleyLevel wi - 1
  where
    bendScaleL ∷ Double
    bendScaleL = 240

    bendScaleS ∷ Double
    bendScaleS = 80

-- | Return the original probability adjusted for the multiplier to
-- the confidence ratio. Useful for computationally cheap remappings
-- within @[0, 1]@.
bayesianAdjustment ∷ (Floating r, Ord r) ⇒ r → r → r
{-# SPECIALISE bayesianAdjustment ∷ Double → Double → Double #-}
bayesianAdjustment probability multiplier
  | probability ≥ 1 = 1
  | probability ≤ 0 = 0
  | otherwise
    = let newConfidence = probability ⋅ multiplier / (1 - probability)
      in
        newConfidence / (1 + newConfidence)

-- | Compute the climate for a given @(x, y)@ coordinates. The @z@
-- coordinate is ignored.
climate ∷ Member (Reader WorldInfo) r ⇒ Double → WorldPos → Eff r Climate
climate h pos
  = do let pX0 = fromIntegral (pos^.wpX)
           pY0 = fromIntegral (pos^.wpY)
           h'  = realToFrac h

       wi ← ask
       let scale = realToFrac (wiBiomeSize wi)

       Scalar tempF  ← (\s → simplex2D s (pX0 / scale) (pY0 / scale)) <$> simplexInstance 2
       Scalar humidF ← (\s → simplex2D s (pX0 / scale) (pY0 / scale)) <$> simplexInstance 3

       -- Now we obtained temperature and humidity in [-1, 1]. Remap
       -- these values to appropriate ranges.
       let baseTemp  = mix minTemp  maxTemp  ((tempF  + 1) / 2)
           baseHumid = mix minHumid maxHumid ((humidF + 1) / 2)

       -- Then adjust these values depending on the altitude. IRL
       -- temperature drops 0.649℃ per 100m, and relative humidity
       -- drops 4% per 1km. But our altitude is in [-1, 1] so we have
       -- to reinterpret that to some convincing range.
       let altitude
             | h ≤ 0     = mix minAlt 0 (h' + 1)
             | otherwise = mix 0 maxAlt  h'

       let temp  = baseTemp  - (altitude /  100) ⋅ 0.649
           humid = baseHumid - (altitude / 1000) ⋅ 0.04

       return $ Climate
         { cliTemperature = max temp (-273.15)
         , cliHumidity    = clamp humid 0 1
         , cliAltitude    = altitude
         }
  where
    minTemp, maxTemp ∷ Float
    minTemp = -20
    maxTemp = 40

    minHumid, maxHumid ∷ Float
    minHumid = 0
    maxHumid = 1

    -- Min/max altitude in meters. These values are far from realistic
    -- but meh…
    minAlt, maxAlt ∷ Float
    minAlt = -500
    maxAlt = 3000

-- | Linear interpolation; same as GLSL @mix()@. The third argument
-- should be in [0, 1] to give a useful result, but it doesn't have to
-- be.
mix ∷ Num r ⇒ r → r → r → r
{-# INLINE mix #-}
mix lo hi r
  = lo⋅(1-r) + hi⋅r

-- | Constrainment; same as GLSL @clamp()@.
clamp ∷ Ord r ⇒ r → r → r → r
{-# INLINE clamp #-}
clamp r lo hi
  = min (max r lo) hi
