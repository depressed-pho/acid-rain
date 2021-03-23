{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen.SpaceAttrs
  ( SpaceAttrs(..)

    -- * Computing space attributes
  , spaceAttrs

    -- * Querying space attributes
  , remappedHeight
  , isWaterLogged
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.Reader.Lazy (Reader, ask)
import Data.Int (Int8)
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo
  ( WorldInfo(..), simplexInstance, voronoiInstance )
import Game.AcidRain.World.Position (WorldPos, wpX, wpY, lowestZ)
import Numeric.Noise.FBM (fBm)
import Numeric.Noise.OpenSimplex (SimplexGen, simplex2D, scalar, diskX, diskY)
import Numeric.Noise.Voronoi (voronoi2D, interiorValue)
import Lens.Micro ((^.))
import Prelude.Unicode ((⋅), (≥), (≤))


-- | Space attributes describe a set of attributes determined for each
-- @(x, y)@ coordinates in a world.
data SpaceAttrs
  = SpaceAttrs
    { -- | The height at this point in @[-1, 1]@. It is generated with
      -- a domain-warped fBm (see
      -- <https://github.com/dandrino/terrain-erosion-3-ways>) and is
      -- then further affected by a river strength which is computed
      -- using a jittered Voronoi noise.
      saHeight ∷ !Double
    } deriving (Show)

-- | Get a remapped height in @[-1, 0]@. The sea level becomes @-1@
-- after remapping, and nearly every height is also remapped to
-- @-1@. Only a few become @0@.
remappedHeight ∷ SpaceAttrs → Int8
remappedHeight sa
  = let height0 = saHeight sa
    in
      -- Remap everything below the sea level to lowestZ.
      if height0 ≤ seaLevel then
        lowestZ
      else
        -- And now this is a hard question. How exactly should we
        -- remap heights above the sea level? For now we simply apply
        -- a cut off at a certain constant height.
        if height0 ≤ cutOff then
          lowestZ
        else
          lowestZ + 1
  where
    cutOff = 0.52 --0.78

-- | Test if the height of the ground is blow the sea level. Whether
-- the water is seawater or not is outside of the scope of this
-- function.
isWaterLogged ∷ SpaceAttrs → Bool
isWaterLogged sa
  = saHeight sa ≤ seaLevel

-- | Compute 'SpaceAttrs' for a given @(x, y)@ coordinates. The @z@
-- coordinate is ignored.
spaceAttrs ∷ Member (Reader WorldInfo) r ⇒ WorldPos → Eff r SpaceAttrs
spaceAttrs pos
  = do h ← height pos
       return SpaceAttrs
         { saHeight = h
         }

-- | Compute the final height for a given (x, y) coordinates. For the
-- base height we compute @fBm(p + fBm(p + fBm(p)))@ for the point
-- @p@.
height ∷ Member (Reader WorldInfo) r ⇒ WorldPos → Eff r Double
height pos
  = do let pt0 = (fromIntegral (pos^.wpX), fromIntegral (pos^.wpY))

       noiseX  ← simplexInstance 0
       noiseY  ← simplexInstance 1
       let pt1 = ( evalFBm noiseX pt0
                 , evalFBm noiseY pt0 )

       noiseX' ← simplexInstance 2
       noiseY' ← simplexInstance 3
       let pt2 = ( evalFBm noiseX' (offset pt0 (scale warp pt1))
                 , evalFBm noiseY' (offset pt0 (scale warp pt1)) )

       noiseF  ← simplexInstance 4
       let baseHeight = evalFBm noiseF (offset pt0 (scale warp pt2))

       -- Now we obtained a height in [-1, 1]. Enhance mountains and
       -- valleys by multiplying it by its absolute value on the power
       -- of some constant.
       wi      ← ask
       let enhancedHeight
             = baseHeight⋅(abs baseHeight)⋅(wiMountainExp wi)

       -- Then apply a river strength.
       river   ← riverStrength pos
       return $ riverize enhancedHeight river
  where
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

-- | The sea level.
seaLevel ∷ Double
seaLevel = 0

-- | Corrode the terrain based on the 'riverStrength'.
riverize ∷ Double → Double → Double
riverize baseHeight river
  | baseHeight ≤ seaLevel
    = baseHeight -- Already sunk in water.
  | otherwise
    = let adjustment = (baseHeight - seaLevel) * 6.4 + 0.6
          river'     = bayesianAdjustment (river + 1) adjustment
      in
        -- Adjustment to make riverbanks more varied depending on
        -- height. The lower the height is, the wider the river should
        -- be.
        seaLevel + (baseHeight - seaLevel) ⋅ river'

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
