{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | An implementation of Voronoi noise, based on the code from
-- <https://github.com/Team-RTG/Realistic-Terrain-Generation Realistic-Terrain-Generation>.
module Numeric.Noise.Voronoi
  ( -- * Types
    VoronoiGen
  , Voronoi2D(..)
  , borderValue
  , interiorValue

    -- * Constructing noise generator
  , mkVoronoiGen

    -- * Generating noise
  , voronoi2D
  ) where

import Control.Eff (run)
import Control.Eff.State.Strict (execState, modify, get)
import Control.Monad (when)
import Control.Monad.Primitive (MonadPrim)
import Control.Monad.ST (ST, runST)
import Data.Default (Default(..))
import Data.Hashable (Hashable, hash)
import Data.Int (Int64)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import Prelude.Unicode ((≥), (≢), (⋅))
import System.Random (StdGen, UniformRange, mkStdGen, uniform)
import System.Random.Stateful (newSTGenM, uniformRM)


-- | Voronoi noise generator. It is constructed with 'mkVoronoiGen'
-- and is immutable.
data VoronoiGen r
  = VoronoiGen
    { -- | Points in [0, 1] for both x and y coordinates.
      vgAllPoints ∷ !(UV.Vector (r, r))
    }

-- | Result of 2D Voronoi noise. It is generated with 'voronoi2D'.
data Voronoi2D r
  = Voronoi2D
    { -- | Squared distance to the closest point. A point of a Voronoi
      -- diagram is the middle of a cell.
      v2dShortestDistanceSq ∷ !r
      -- | Squared distance to the next closest point.
    , v2dNextDistanceSq ∷ !r
      -- | @(x, y)@ coordinates of the closest point.
    , v2dClosestPoint ∷ !(r, r)
    } deriving Show

instance Fractional r ⇒ Default (Voronoi2D r) where
  {-# SPECIALISE instance Default (Voronoi2D Double) #-}
  def = Voronoi2D
        { v2dShortestDistanceSq = 1 / 0 -- Infinity
        , v2dNextDistanceSq     = 1 / 0
        , v2dClosestPoint       = (1 / 0, 1 / 0)
        }

totalPoints ∷ Int
totalPoints = 100

pointsPerTorus ∷ Int
pointsPerTorus = 25

minDistanceSq ∷ Floating r ⇒ r
minDistanceSq = 0.005

-- | Construct a Voronoi noise generator with a given 64-bits
-- seed. Since this is quite an expensive operation, one should reuse
-- generators as far as possible.
mkVoronoiGen ∷ (Floating r, Ord r, UV.Unbox r, UniformRange r) ⇒ Int64 → VoronoiGen r
{-# SPECIALISE mkVoronoiGen ∷ Int64 → VoronoiGen Double #-}
mkVoronoiGen seed
  = runST $
    do allPoints ← GMV.unsafeNew totalPoints

       xRandST ← newSTGenM xRandom
       yRandST ← newSTGenM yRandom
       let go i
             | i ≥ totalPoints = return ()
             | otherwise
               = do x  ← uniformRM (0, 1) xRandST
                    y  ← uniformRM (0, 1) yRandST
                    ds ← minimalToroidalDistanceSquared (x, y) allPoints i
                    if ds ≥ minDistanceSq
                      then do GMV.write allPoints i (x, y)
                              go (i + 1)
                      else go i -- Retry with the next random point.
       go 0

       allPoints' ← GV.freeze allPoints
       return VoronoiGen
         { vgAllPoints = allPoints'
         }
  where
    xRandom ∷ StdGen
    xRandom = mkStdGen (hash seed)

    yRandom ∷ StdGen
    yRandom = case uniform xRandom of
                (ySeed, _) → mkStdGen ySeed

minimalToroidalDistanceSquared ∷ (Floating r, Ord r, GMV.MVector v (r, r), MonadPrim σ μ)
                               ⇒ (r, r)
                               → v σ (r, r)
                               → Int → μ r
{-# SPECIALISE minimalToroidalDistanceSquared
    ∷ (Double, Double) → UV.MVector σ (Double, Double) → Int → ST σ Double #-}
minimalToroidalDistanceSquared point existing numPoints = go 0 1.0
  where
    go i result
      | i ≥ numPoints = return result
      | otherwise     = do point' ← GMV.read existing i
                           case toroidalDistanceSquared point point' of
                             distSq
                               | distSq < result → go (i + 1) distSq
                               | otherwise       → go (i + 1) result

toroidalDistanceSquared ∷ (Floating r, Ord r) ⇒ (r, r) → (r, r) → r
{-# SPECIALISE toroidalDistanceSquared
    ∷ (Double, Double) → (Double, Double) → Double #-}
toroidalDistanceSquared (x0, y0) (x1, y1)
  = let xDist = case abs (x0 - x1) of
                  xDist0
                    | xDist0 > 0.5 → 1.0 - xDist0
                    | otherwise    → xDist0
        yDist = case abs (y0 - y1) of
                  yDist0
                    | yDist0 > 0.5 → 1.0 - yDist0
                    | otherwise    → yDist0
    in
      xDist ⋅ xDist + yDist ⋅ yDist

voronoi2D ∷ (Floating r, RealFrac r, UV.Unbox r)
          ⇒ VoronoiGen r
          → r -- ^ x
          → r -- ^ y
          → Voronoi2D r
{-# SPECIALISE voronoi2D ∷ VoronoiGen Double → Double → Double → Voronoi2D Double #-}
voronoi2D gen x0 y0
  = run $ execState def $
    do let -- This algorithm places the points about five times more
           -- frequently, so adjust the passed coordinates rather than
           -- recalibrating all the routes.
           x    = x0 / 5
           y    = y0 / 5
           pt   = (x, y)

           xInt = floor x ∷ Int
           yInt = floor y ∷ Int

       -- Evaluate the points for the square (xInt, yInt).
       modify $ evalPoint (areaPoints gen (xInt, yInt)) pt

       -- Now horizontally adjacent squares as appropriate
       -- FIXME: Do these tests really make sense? I don't think so.
       case y - fromIntegral yInt of
         distSq → do res ← get
                     when (distSq ≢ v2dNextDistanceSq res) $
                       modify $ evalPoint (areaPoints gen (xInt, yInt - 1)) pt

       case x - fromIntegral xInt of
         distSq → do res ← get
                     when (distSq ≢ v2dNextDistanceSq res) $
                       modify $ evalPoint (areaPoints gen (xInt - 1, yInt)) pt

       case y - fromIntegral yInt + 1 of
         distSq → do res ← get
                     when (distSq ≢ v2dNextDistanceSq res) $
                       modify $ evalPoint (areaPoints gen (xInt, yInt + 1)) pt

       case x - fromIntegral xInt + 1 of
         distSq → do res ← get
                     when (distSq ≢ v2dNextDistanceSq res) $
                       modify $ evalPoint (areaPoints gen (xInt + 1, yInt)) pt

       -- Now diagonally adjacent squares
       case min (y - fromIntegral yInt) (x - fromIntegral xInt) of
         distSq → do res ← get
                     when (distSq ≢ v2dNextDistanceSq res) $
                       modify $ evalPoint (areaPoints gen (xInt - 1, yInt - 1)) pt

       case min (y - fromIntegral yInt + 1) (x - fromIntegral xInt) of
         distSq → do res ← get
                     when (distSq ≢ v2dNextDistanceSq res) $
                       modify $ evalPoint (areaPoints gen (xInt - 1, yInt + 1)) pt

       case min (y - fromIntegral yInt + 1) (x - fromIntegral xInt + 1) of
         distSq → do res ← get
                     when (distSq ≢ v2dNextDistanceSq res) $
                       modify $ evalPoint (areaPoints gen (xInt + 1, yInt + 1)) pt

       case min (y - fromIntegral yInt) (x - fromIntegral xInt + 1) of
         distSq → do res ← get
                     when (distSq ≢ v2dNextDistanceSq res) $
                       modify $ evalPoint (areaPoints gen (xInt + 1, yInt - 1)) pt

-- Create a set of points on a unit torus, none of which are too
-- close. Each unit square gets a random subset of these points.
areaPoints ∷ (Integral i, Floating r, Hashable i, UV.Unbox r)
           ⇒ VoronoiGen r
           → (i, i) -- ^ area
           → UV.Vector (r, r)
{-# SPECIALISE areaPoints ∷ VoronoiGen Double → (Int, Int) → UV.Vector (Double, Double) #-}
areaPoints gen area@(areaX, areaY)
  = runST $
    do random ← newSTGenM $ mkStdGen (hash area)
       used   ← GMV.replicate totalPoints False ∷ ST σ (UV.MVector σ Bool)
       result ← GMV.unsafeNew pointsPerTorus

       let choosePoints index i
             | i ≥ pointsPerTorus = return ()
             | otherwise
               = do advance ← uniformRM (0, totalPoints-1) random
                    let findUnused index0 j
                          | j ≥ advance = return index0
                          | otherwise
                            = do u ← GMV.read used index0
                                 if u
                                   then case index0 + 1 of
                                          index1 | index1 ≥ totalPoints → findUnused 0 j
                                                 | otherwise            → findUnused index1 j
                                   else findUnused index0 (j + 1)
                    index'  ← findUnused index 0
                    -- Add the point, offset to the area.
                    let (x, y) = (vgAllPoints gen) GV.! index'
                    GMV.write result i (x + fromIntegral areaX, y + fromIntegral areaY)
                    -- And mark the point as used.
                    GMV.write used index' True
                    choosePoints index' (i + 1)
       choosePoints 0 0

       GV.freeze result

evalPoint ∷ ∀r. (Floating r, Ord r, UV.Unbox r)
          ⇒ UV.Vector (r, r)
          → (r, r)
          → Voronoi2D r
          → Voronoi2D r
{-# SPECIALISE evalPoint
    ∷ UV.Vector (Double, Double) → (Double, Double) → Voronoi2D Double → Voronoi2D Double #-}
evalPoint points (x, y) res0 = GV.foldl' update res0 points
  where
    update ∷ Voronoi2D r → (r, r) → Voronoi2D r
    update res (pX, pY)
      = let xDist  = x - pX
            yDist  = y - pY
        in
          case xDist ⋅ xDist + yDist ⋅ yDist of
            distSq
              | distSq < v2dShortestDistanceSq res →
                  res {
                    v2dNextDistanceSq     = v2dShortestDistanceSq res
                  , v2dShortestDistanceSq = distSq
                  , v2dClosestPoint       = (pX, pY)
                  }
              | otherwise →
                  res {
                    v2dNextDistanceSq = distSq
                  }

-- | Return 0 in the middle of a cell and 1 on the border.
borderValue ∷ Fractional r ⇒ Voronoi2D r → r
{-# SPECIALISE borderValue ∷ Voronoi2D Double → Double #-}
borderValue res
  = (v2dShortestDistanceSq res) / (v2dNextDistanceSq res)

-- | Return 1 in the middle of a cell and 0 on the border.
interiorValue ∷ Fractional r ⇒ Voronoi2D r → r
{-# SPECIALISE interiorValue ∷ Voronoi2D Double → Double #-}
interiorValue res
  = (v2dNextDistanceSq res - v2dShortestDistanceSq res) / (v2dNextDistanceSq res)
