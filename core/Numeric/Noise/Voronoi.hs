{-# LANGUAGE BangPatterns #-}
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

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Cache.LRU (LRU)
import qualified Control.Concurrent.Cache.LRU as LRU
import Control.Monad (when)
import Control.Monad.Primitive (MonadPrim)
import Control.Monad.ST (ST, runST)
import Data.Hashable (Hashable, hash)
import Data.Int (Int64)
import Data.STRef (STRef, newSTRef, writeSTRef, readSTRef)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import Prelude.Unicode ((≥), (≢), (⋅), (∘))
import System.IO.Unsafe (unsafePerformIO)
import System.Random (StdGen, UniformRange, mkStdGen, uniform)
import System.Random.Stateful (newSTGenM, uniformRM)


-- | Voronoi noise generator. It is constructed with 'mkVoronoiGen'
-- and is immutable.
data VoronoiGen r
  = VoronoiGen
    { -- | Points in [0, 1] for both x and y coordinates.
      vgAllPoints ∷ !(UV.Vector (r, r))
      -- | Cache of 'areaPoints'.
    , vgAreaCache ∷ !(LRU (r, r) (UV.Vector (r, r)))
    }

-- | Result of 2D Voronoi noise. It is generated with 'voronoi2D'.
data Voronoi2D r
  = Voronoi2D
    { -- | Squared distance to the closest point. A point of a Voronoi
      -- diagram is the middle of a cell.
      v2dShortestDistanceSq ∷ !r
      -- | Squared distance to the next closest point.
    , v2dNextDistanceSq ∷ !r
      -- | @x@ coordinate of the closest point.
    , v2dClosestX ∷ !r
      -- | @y@ coordinate of the closest point.
    , v2dClosestY ∷ !r
    } deriving Show

data MVoronoi2D σ r
  = MVoronoi2D
    { mv2dShortestDistanceSq ∷ !(STRef σ r)
    , mv2dNextDistanceSq ∷ !(STRef σ r)
    , mv2dClosestX ∷ !(STRef σ r)
    , mv2dClosestY ∷ !(STRef σ r)
    }

newMutableV2D ∷ Fractional r ⇒ ST σ (MVoronoi2D σ r)
newMutableV2D
  = do shortest ← newSTRef (1/0) -- Infinity
       next     ← newSTRef (1/0)
       closestX ← newSTRef (1/0)
       closestY ← newSTRef (1/0)
       return MVoronoi2D
         { mv2dShortestDistanceSq = shortest
         , mv2dNextDistanceSq     = next
         , mv2dClosestX           = closestX
         , mv2dClosestY           = closestY
         }

freezeMutableV2D ∷ MVoronoi2D σ r → ST σ (Voronoi2D r)
freezeMutableV2D mv
  = do shortest ← readSTRef $ mv2dShortestDistanceSq mv
       next     ← readSTRef $ mv2dNextDistanceSq mv
       closestX ← readSTRef $ mv2dClosestX mv
       closestY ← readSTRef $ mv2dClosestY mv
       return Voronoi2D
         { v2dShortestDistanceSq = shortest
         , v2dNextDistanceSq     = next
         , v2dClosestX           = closestX
         , v2dClosestY           = closestY
         }

totalPoints ∷ Int
totalPoints = 100

pointsPerTorus ∷ Int
pointsPerTorus = 25

minDistanceSq ∷ Floating r ⇒ r
{-# INLINE minDistanceSq #-}
minDistanceSq = 0.005

-- | Construct a Voronoi noise generator with a given 64-bits
-- seed. Since this is quite an expensive operation, one should reuse
-- generators as far as possible.
mkVoronoiGen ∷ (Hashable h, Hashable r, Floating r, Ord r, UV.Unbox r, UniformRange r)
             ⇒ h
             → VoronoiGen r
{-# SPECIALISE mkVoronoiGen ∷ Int64 → VoronoiGen Float  #-}
{-# SPECIALISE mkVoronoiGen ∷ Int64 → VoronoiGen Double #-}
mkVoronoiGen seed
  = runST $
    do allPoints ← GMV.unsafeNew totalPoints

       xRandST ← newSTGenM xRandom
       yRandST ← newSTGenM yRandom
       let go !i
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

       allPoints' ← GV.unsafeFreeze allPoints
       let gen = VoronoiGen
                 { vgAllPoints = allPoints'
                 , vgAreaCache = unsafePerformIO $
                                 do caps ← getNumCapabilities
                                    LRU.new caps 256 (return ∘ genAreaPoints gen)
                                                     (const $ const $ return ())
                 }
       return gen
  where
    xRandom ∷ StdGen
    xRandom = mkStdGen (hash seed)

    yRandom ∷ StdGen
    yRandom = case uniform xRandom of
                (ySeed, _) → mkStdGen ySeed

minimalToroidalDistanceSquared ∷ (Floating r, Ord r, GMV.MVector v (r, r), MonadPrim σ μ)
                               ⇒ (r, r)
                               → v σ (r, r)
                               → Int
                               → μ r
{-# SPECIALISE minimalToroidalDistanceSquared
    ∷ (Float , Float ) → UV.MVector σ (Float , Float ) → Int → ST σ Float  #-}
{-# SPECIALISE minimalToroidalDistanceSquared
    ∷ (Double, Double) → UV.MVector σ (Double, Double) → Int → ST σ Double #-}
minimalToroidalDistanceSquared !point !existing !numPoints = go 0 1.0
  where
    go !i !result
      | i ≥ numPoints = return result
      | otherwise     = do point' ← GMV.read existing i
                           case toroidalDistanceSquared point point' of
                             distSq
                               | distSq < result → go (i + 1) distSq
                               | otherwise       → go (i + 1) result

toroidalDistanceSquared ∷ (Floating r, Ord r) ⇒ (r, r) → (r, r) → r
{-# SPECIALISE toroidalDistanceSquared
    ∷ (Float , Float ) → (Float , Float ) → Float  #-}
{-# SPECIALISE toroidalDistanceSquared
    ∷ (Double, Double) → (Double, Double) → Double #-}
toroidalDistanceSquared (!x0, !y0) (!x1, !y1)
  = let !xDist = case abs (x0 - x1) of
                   xDist0
                     | xDist0 > 0.5 → 1.0 - xDist0
                     | otherwise    → xDist0
        !yDist = case abs (y0 - y1) of
                   yDist0
                     | yDist0 > 0.5 → 1.0 - yDist0
                     | otherwise    → yDist0
    in
      xDist⋅xDist + yDist⋅yDist

voronoi2D ∷ (Hashable r, Floating r, RealFrac r, UV.Unbox r)
          ⇒ VoronoiGen r
          → r -- ^ x
          → r -- ^ y
          → Voronoi2D r
{-# SPECIALISE voronoi2D ∷ VoronoiGen Float  → Float  → Float  → Voronoi2D Float  #-}
{-# SPECIALISE voronoi2D ∷ VoronoiGen Double → Double → Double → Voronoi2D Double #-}
voronoi2D gen x0 y0
  = runST $
    do mv ← newMutableV2D

       -- This algorithm places the points about five times more
       -- frequently, so adjust the passed coordinates rather than
       -- recalibrating all the routes.
       let !x    = x0 / 5
           !y    = y0 / 5
           !pt   = (x, y)

           !xInt = fromInteger (floor x)
           !yInt = fromInteger (floor y)

       -- Evaluate the points for the square (xInt, yInt).
       evalPoint mv (areaPoints gen (xInt, yInt)) pt

       -- Now horizontally adjacent squares as appropriate
       -- FIXME: Do these tests really make sense? I don't think so.
       case y - yInt of
         distSq → do next ← readSTRef (mv2dNextDistanceSq mv)
                     when (distSq ≢ next) $
                       evalPoint mv (areaPoints gen (xInt, yInt - 1)) pt

       case x - xInt of
         distSq → do next ← readSTRef (mv2dNextDistanceSq mv)
                     when (distSq ≢ next) $
                       evalPoint mv (areaPoints gen (xInt - 1, yInt)) pt

       case y - yInt + 1 of
         distSq → do next ← readSTRef (mv2dNextDistanceSq mv)
                     when (distSq ≢ next) $
                       evalPoint mv (areaPoints gen (xInt, yInt + 1)) pt

       case x - xInt + 1 of
         distSq → do next ← readSTRef (mv2dNextDistanceSq mv)
                     when (distSq ≢ next) $
                       evalPoint mv (areaPoints gen (xInt + 1, yInt)) pt

       -- Now diagonally adjacent squares
       case min (y - yInt) (x - xInt) of
         distSq → do next ← readSTRef (mv2dNextDistanceSq mv)
                     when (distSq ≢ next) $
                       evalPoint mv (areaPoints gen (xInt - 1, yInt - 1)) pt

       case min (y - yInt + 1) (x - xInt) of
         distSq → do next ← readSTRef (mv2dNextDistanceSq mv)
                     when (distSq ≢ next) $
                       evalPoint mv (areaPoints gen (xInt - 1, yInt + 1)) pt

       case min (y - yInt + 1) (x - xInt + 1) of
         distSq → do next ← readSTRef (mv2dNextDistanceSq mv)
                     when (distSq ≢ next) $
                       evalPoint mv (areaPoints gen (xInt + 1, yInt + 1)) pt

       case min (y - yInt) (x - xInt + 1) of
         distSq → do next ← readSTRef (mv2dNextDistanceSq mv)
                     when (distSq ≢ next) $
                       evalPoint mv (areaPoints gen (xInt + 1, yInt - 1)) pt

       freezeMutableV2D mv

areaPoints ∷ (Eq r, Floating r, Hashable r, UV.Unbox r)
           ⇒ VoronoiGen r
           → (r, r) -- ^ area
           → UV.Vector (r, r)
{-# SPECIALISE areaPoints
    ∷ VoronoiGen Float  → (Float , Float ) → UV.Vector (Float , Float ) #-}
{-# SPECIALISE areaPoints
    ∷ VoronoiGen Double → (Double, Double) → UV.Vector (Double, Double) #-}
areaPoints gen area
  = unsafePerformIO $
    LRU.lookup area (vgAreaCache gen)

-- Create a set of points on a unit torus, none of which are too
-- close. Each unit square gets a random subset of these points.
genAreaPoints ∷ (Floating r, Hashable r, UV.Unbox r)
              ⇒ VoronoiGen r
              → (r, r) -- ^ area
              → UV.Vector (r, r)
{-# SPECIALISE genAreaPoints
    ∷ VoronoiGen Float  → (Float , Float ) → UV.Vector (Float , Float ) #-}
{-# SPECIALISE genAreaPoints
    ∷ VoronoiGen Double → (Double, Double) → UV.Vector (Double, Double) #-}
genAreaPoints gen area@(areaX, areaY)
  = runST $
    do random ← newSTGenM $ mkStdGen (hash area)
       used   ← GMV.replicate totalPoints False ∷ ST σ (UV.MVector σ Bool)
       result ← GMV.unsafeNew pointsPerTorus

       let choosePoints !index !i
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
                    GMV.write result i (x + areaX, y + areaY)
                    -- And mark the point as used.
                    GMV.write used index' True
                    choosePoints index' (i + 1)
       choosePoints 0 0

       GV.unsafeFreeze result

evalPoint ∷ ∀r σ. (Floating r, Ord r, UV.Unbox r)
          ⇒ MVoronoi2D σ r
          → UV.Vector (r, r)
          → (r, r)
          → ST σ ()
{-# SPECIALISE evalPoint
    ∷ MVoronoi2D σ Float  → UV.Vector (Float , Float ) → (Float , Float ) → ST σ () #-}
{-# SPECIALISE evalPoint
    ∷ MVoronoi2D σ Double → UV.Vector (Double, Double) → (Double, Double) → ST σ () #-}
evalPoint !mv !points (!x, !y) = GV.mapM_ update points
  where
    update ∷ (r, r) → ST σ ()
    update (!pX, !pY)
      = do let !xDist = x - pX
               !yDist = y - pY
           shortest ← readSTRef $ mv2dShortestDistanceSq mv
           case xDist ⋅ xDist + yDist ⋅ yDist of
             distSq
               | distSq < shortest →
                   do writeSTRef (mv2dNextDistanceSq     mv) shortest
                      writeSTRef (mv2dShortestDistanceSq mv) distSq
                      writeSTRef (mv2dClosestX           mv) pX
                      writeSTRef (mv2dClosestY           mv) pY
               | otherwise →
                   writeSTRef (mv2dNextDistanceSq mv) distSq

-- | Return 0 in the middle of a cell and 1 on the border.
borderValue ∷ Fractional r ⇒ Voronoi2D r → r
{-# SPECIALISE borderValue ∷ Voronoi2D Float  → Float  #-}
{-# SPECIALISE borderValue ∷ Voronoi2D Double → Double #-}
borderValue res
  = (v2dShortestDistanceSq res) / (v2dNextDistanceSq res)

-- | Return 1 in the middle of a cell and 0 on the border.
interiorValue ∷ Fractional r ⇒ Voronoi2D r → r
{-# SPECIALISE interiorValue ∷ Voronoi2D Float  → Float  #-}
{-# SPECIALISE interiorValue ∷ Voronoi2D Double → Double #-}
interiorValue res
  = (v2dNextDistanceSq res - v2dShortestDistanceSq res) / (v2dNextDistanceSq res)
