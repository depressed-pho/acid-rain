{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen.SpaceAttr
  ( SpaceAttr(..)
  , spaceAttr
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.Reader.Lazy (Reader, ask)
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo
  ( WorldInfo(..), simplexInstance, voronoiInstance )
import Game.AcidRain.World.Position (WorldPos, wpX, wpY)
import Numeric.Noise.OpenSimplex (simplex2D, diskX, diskY)
import Numeric.Noise.Voronoi (voronoi2D, interiorValue)
import Lens.Micro ((^.))
import Prelude.Unicode ((⋅), (≥), (≤))


-- | Space attribute describes a set of variables determined for each
-- @(x, y)@ coordinates in a world.
data SpaceAttr
  = SpaceAttr
    { -- | The height at this point. It is generated with a
      -- domain-warped fBm (see
      -- <https://github.com/dandrino/terrain-erosion-3-ways>) and is
      -- then further affected by a river strength which is computed
      -- using a jittered Voronoi noise.
      saHeight ∷ !Double
    } deriving (Show)

-- | Compute a 'SpaceAttr' for the given @(x, y)@ coordinates. The @z@
-- coordinate is ignored.
spaceAttr ∷ Member (Reader WorldInfo) r ⇒ WorldPos → Eff r SpaceAttr
spaceAttr pos
  = error "FIXME"

-- | The sea level.
seaLevel ∷ Double
seaLevel = 63

-- | Corrode the terrain based on the 'riverStrength'.
riverize ∷ Double → Double → Double
riverize height river
  | height < seaLevel' - 0.55
    = height -- Already sunk in water.
  | otherwise
    = let adjustment = (height - seaLevel) / 10 + 0.6
          river'     = bayesianAdjustment (river + 1) adjustment
      in
        -- Adjustment to make riverbanks more varied.
        seaLevel' + (height - seaLevel') ⋅ river'
  where
    seaLevel' = seaLevel - 0.55

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
