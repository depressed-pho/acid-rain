{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | An implementation of OpenSimplex noise, based on the code from
-- <https://github.com/Team-RTG/Realistic-Terrain-Generation Realistic-Terrain-Generation>.
module Numeric.Noise.OpenSimplex
  ( -- * Types
    SimplexGen
  , Simplex2D(..)
  , Scalar(..), scalar
  , Disk(..), diskX, diskY
  , Derivative(..), dx, dy

    -- * Constructing noise generator
  , mkSimplexGen

    -- * Generating noise
  , simplex2D
  , simplex3D
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits ((.|.), (.&.), shiftL, xor)
import Data.Default (Default, def)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.STRef (STRef, newSTRef, writeSTRef, readSTRef)
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Lens.Micro ((&), (^.), (+~))
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((⋅), (≡), (≤))


-- | OpenSimplex noise generator. It is constructed with
-- 'mkSimplexGen' and is immutable.
data SimplexGen
  = SimplexGen
    { -- The length of these vectors are all statically known (1024),
      -- but we don't use the sized vector mainly because 'Finite' is
      -- not unboxable. We value efficiency more than safety here.
      _sgPerm       ∷ !(UV.Vector Int)
    , _sgPerm2D     ∷ !(UV.Vector Int)
    , _sgPerm2DSph2 ∷ !(UV.Vector Int)
    , _sgPerm3D     ∷ !(UV.Vector Int)
    }

makeLenses ''SimplexGen

-- | Class of types that can be a result value of 'simplex2D'.
class (Default α, Floating (BaseType α), RealFrac (BaseType α), UV.Unbox (BaseType α)) ⇒ Simplex2D α where
  type BaseType α ∷ Type
  accumulate ∷ α
             → BaseType α -- ^ attn
             → BaseType α -- ^ extrp
             → BaseType α -- ^ gx
             → BaseType α -- ^ gy
             → Int        -- ^ gi_sph2
             → BaseType α -- ^ δx
             → BaseType α -- ^ δy
             → α

-- | An output of 2D OpenSimplex noise: scalar value @[-1, 1]@.
newtype Scalar r
  = Scalar
    { _scalar ∷ r
    }
  deriving (Show, Eq)

makeLenses ''Scalar

-- | An output of 2D OpenSimplex noise: 2D coordinate within a unit
-- disk.
data Disk r
  = Disk
    { _diskX ∷ !r
    , _diskY ∷ !r
    } deriving (Show, Eq)

makeLenses ''Disk

-- | An output of 2D OpenSimplex noise: the first derivative of 2D
-- gradients.
data Derivative r
  = Derivative
    { _dx ∷ !r
    , _dy ∷ !r
    } deriving (Show, Eq)

makeLenses ''Derivative

stretch2D ∷ Floating r ⇒ r
{-# SPECIALISE stretch2D ∷ Float  #-}
{-# SPECIALISE stretch2D ∷ Double #-}
stretch2D = (1 / (sqrt (2 + 1)) - 1) / 2

squish2D ∷ Floating r ⇒ r
{-# SPECIALISE squish2D ∷ Float  #-}
{-# SPECIALISE squish2D ∷ Double #-}
squish2D = (sqrt (2 + 1) - 1) / 2

stretch3D ∷ Floating r ⇒ r
{-# SPECIALISE stretch3D ∷ Float  #-}
{-# SPECIALISE stretch3D ∷ Double #-}
stretch3D = (1 / sqrt (3 + 1) - 1) / 3

squish3D ∷ Floating r ⇒ r
{-# SPECIALISE squish3D ∷ Float  #-}
{-# SPECIALISE squish3D ∷ Double #-}
squish3D = (sqrt (3 + 1) - 1) / 3

data LatticePoint2D r
  = LatticePoint2D
    { _lxsv ∷ !Int
    , _lysv ∷ !Int
    , _lδx  ∷ !r
    , _lδy  ∷ !r
    }

makeLenses ''LatticePoint2D

latticePoint2D ∷ Floating r ⇒ Int → Int → LatticePoint2D r
{-# SPECIALISE latticePoint2D ∷ Int → Int → LatticePoint2D Float  #-}
{-# SPECIALISE latticePoint2D ∷ Int → Int → LatticePoint2D Double #-}
latticePoint2D xsv ysv
  = LatticePoint2D
    { _lxsv = xsv
    , _lysv = ysv
    , _lδx  = (negate xsv') - (xsv' + ysv') ⋅ squish2D
    , _lδy  = (negate ysv') - (xsv' + ysv') ⋅ squish2D
    }
    where
      xsv' = fromIntegral xsv
      ysv' = fromIntegral ysv

-- Derive unboxed vectors for LatticePoint2D.
derivingUnbox "LP2D"
  [t| ∀r. UV.Unbox r ⇒ LatticePoint2D r → (Int, Int, r, r) |]
  [e| \lp                 → (lp^.lxsv, lp^.lysv, lp^.lδx, lp^.lδy) |]
  [e| \(xsv, ysv, δx, δy) → LatticePoint2D xsv ysv δx δy           |]

data Contribution3D r
  = Contribution3D
    { _cxsb  ∷ !Int
    , _cysb  ∷ !Int
    , _czsb  ∷ !Int
    , _cδx   ∷ !r
    , _cδy   ∷ !r
    , _cδz   ∷ !r
    , _cnext ∷ !(Maybe (Contribution3D r))
    }

makeLenses ''Contribution3D

-- Mutable version of Contribution3D.
data MContribution3D σ r
  = MContribution3D
    { _c'xsb  ∷ !Int
    , _c'ysb  ∷ !Int
    , _c'zsb  ∷ !Int
    , _c'δx   ∷ !r
    , _c'δy   ∷ !r
    , _c'δz   ∷ !r
    , _c'next ∷ !(STRef σ (Maybe (MContribution3D σ r)))
    }

makeLenses ''MContribution3D

contribution3D ∷ Floating r ⇒ MContribution3D σ r → ST σ (Contribution3D r)
{-# SPECIALISE contribution3D ∷ MContribution3D σ Float  → ST σ (Contribution3D Float ) #-}
{-# SPECIALISE contribution3D ∷ MContribution3D σ Double → ST σ (Contribution3D Double) #-}
contribution3D c'
  = do next' ← readSTRef (c'^.c'next)
       next  ← traverse contribution3D next'
       return Contribution3D
         { _cxsb  = c'^.c'xsb
         , _cysb  = c'^.c'ysb
         , _czsb  = c'^.c'zsb
         , _cδx   = c'^.c'δx
         , _cδy   = c'^.c'δy
         , _cδz   = c'^.c'δz
         , _cnext = next
         }

contribution3D' ∷ Floating r ⇒ r → Int → Int → Int → ST σ (MContribution3D σ r)
{-# SPECIALISE contribution3D' ∷ Float  → Int → Int → Int → ST σ (MContribution3D σ Float ) #-}
{-# SPECIALISE contribution3D' ∷ Double → Int → Int → Int → ST σ (MContribution3D σ Double) #-}
contribution3D' multiplier xsb ysb zsb
  = do next ← newSTRef Nothing
       return MContribution3D
         { _c'xsb  = xsb
         , _c'ysb  = ysb
         , _c'zsb  = zsb
         , _c'δx   = (negate (fromIntegral xsb)) - multiplier ⋅ squish3D
         , _c'δy   = (negate (fromIntegral ysb)) - multiplier ⋅ squish3D
         , _c'δz   = (negate (fromIntegral zsb)) - multiplier ⋅ squish3D
         , _c'next = next
         }

-- 2D gradients (Dodecagon)
gradients2D ∷ (Floating r, UV.Unbox r) ⇒ UV.Vector r
{-# SPECIALISE NOINLINE gradients2D ∷ UV.Vector Float  #-}
{-# SPECIALISE NOINLINE gradients2D ∷ UV.Vector Double #-}
gradients2D
  = GV.fromList
    [  0.114251372530929,  0.065963060686016
    ,  0.131926121372032,  0.000000000000000
    ,  0.114251372530929, -0.065963060686016
    ,  0.065963060686016, -0.114251372530929
    ,  0.000000000000000, -0.131926121372032
    , -0.065963060686016, -0.114251372530929
    , -0.114251372530929, -0.065963060686016
    , -0.131926121372032, -0.000000000000000
    , -0.114251372530929,  0.065963060686016
    , -0.065963060686016,  0.114251372530929
    , -0.000000000000000,  0.131926121372032
    ,  0.065963060686016,  0.114251372530929 ]

gradientsSph2 ∷ (Floating r, UV.Unbox r) ⇒ UV.Vector r
{-# SPECIALISE NOINLINE gradientsSph2 ∷ UV.Vector Float  #-}
{-# SPECIALISE NOINLINE gradientsSph2 ∷ UV.Vector Double #-}
gradientsSph2
  = GV.fromList
    [  0.000000000000000,  1.000000000000000
    ,  0.500000000000000,  0.866025403784439
    ,  0.866025403784439,  0.500000000000000
    ,  1.000000000000000,  0.000000000000000
    ,  0.866025403784439, -0.500000000000000
    ,  0.500000000000000, -0.866025403784439
    ,  0.000000000000000, -1.000000000000000
    , -0.500000000000000, -0.866025403784439
    , -0.866025403784439, -0.500000000000000
    , -1.000000000000000,  0.000000000000000
    , -0.866025403784439,  0.500000000000000
    , -0.500000000000000,  0.866025403784439 ]

-- 3D Gradients (Normalized expanded cuboctahedron)
gradients3D ∷ (Floating r, UV.Unbox r) ⇒ UV.Vector r
{-# SPECIALISE NOINLINE gradients3D ∷ UV.Vector Float  #-}
{-# SPECIALISE NOINLINE gradients3D ∷ UV.Vector Double #-}
gradients3D
  = GV.fromList
    [ -0.009192019279820,  0.061948581592974,  0.105513124626310
    ,  0.061948581592974, -0.009192019279820,  0.105513124626310
    ,  0.052339395980958,  0.052339395980958,  0.097858646551677
    ,  0.002784312704445,  0.002784312704445,  0.122636188189934
    , -0.009192019279820,  0.105513124626310,  0.061948581592974
    ,  0.061948581592974,  0.105513124626310, -0.009192019279820
    ,  0.052339395980958,  0.097858646551677,  0.052339395980958
    ,  0.002784312704445,  0.122636188189934,  0.002784312704445
    ,  0.105513124626310, -0.009192019279820,  0.061948581592974
    ,  0.105513124626310,  0.061948581592974, -0.009192019279820
    ,  0.097858646551677,  0.052339395980958,  0.052339395980958
    ,  0.122636188189934,  0.002784312704445,  0.002784312704445
    , -0.067278076657600,  0.090991610281865,  0.047427067248529
    , -0.090991610281865,  0.067278076657600, -0.047427067248529
    , -0.057908021389848,  0.107463104666361, -0.012388770819128
    , -0.107463104666361,  0.057908021389848,  0.012388770819128
    , -0.067278076657600,  0.047427067248529,  0.090991610281865
    , -0.090991610281865, -0.047427067248529,  0.067278076657600
    , -0.057908021389848, -0.012388770819128,  0.107463104666361
    , -0.107463104666361,  0.012388770819128,  0.057908021389848
    ,  0.047427067248529, -0.067278076657600,  0.090991610281865
    , -0.047427067248529, -0.090991610281865,  0.067278076657600
    , -0.012388770819128, -0.057908021389848,  0.107463104666361
    ,  0.012388770819128, -0.107463104666361,  0.057908021389848
    ,  0.067278076657600, -0.090991610281865, -0.047427067248529
    ,  0.090991610281865, -0.067278076657600,  0.047427067248529
    ,  0.107463104666361, -0.057908021389848, -0.012388770819128
    ,  0.057908021389848, -0.107463104666361,  0.012388770819128
    ,  0.067278076657600, -0.047427067248529, -0.090991610281865
    ,  0.090991610281865,  0.047427067248529, -0.067278076657600
    ,  0.107463104666361, -0.012388770819128, -0.057908021389848
    ,  0.057908021389848,  0.012388770819128, -0.107463104666361
    , -0.047427067248529,  0.067278076657600, -0.090991610281865
    ,  0.047427067248529,  0.090991610281865, -0.067278076657600
    , -0.012388770819128,  0.107463104666361, -0.057908021389848
    ,  0.012388770819128,  0.057908021389848, -0.107463104666361
    ,  0.009192019279820, -0.061948581592974, -0.105513124626310
    , -0.061948581592974,  0.009192019279820, -0.105513124626310
    , -0.002784312704445, -0.002784312704445, -0.122636188189934
    , -0.052339395980958, -0.052339395980958, -0.097858646551677
    ,  0.009192019279820, -0.105513124626310, -0.061948581592974
    , -0.061948581592974, -0.105513124626310,  0.009192019279820
    , -0.002784312704445, -0.122636188189934, -0.002784312704445
    , -0.052339395980958, -0.097858646551677, -0.052339395980958
    , -0.105513124626310,  0.009192019279820, -0.061948581592974
    , -0.105513124626310, -0.061948581592974,  0.009192019279820
    , -0.122636188189934, -0.002784312704445, -0.002784312704445
    , -0.097858646551677, -0.052339395980958, -0.052339395980958 ]

instance Num r ⇒ Default (Scalar r) where
  def = Scalar 0

instance (Floating r, RealFrac r, UV.Unbox r) ⇒ Simplex2D (Scalar r) where
  {-# SPECIALISE instance Simplex2D (Scalar Float ) #-}
  {-# SPECIALISE instance Simplex2D (Scalar Double) #-}
  type BaseType (Scalar r) = r
  accumulate v attn extrp _ _ _ _ _
    = let attnSq = attn ⋅ attn
          extrp' = attnSq ⋅ attnSq ⋅ extrp
      in
        v & scalar +~ extrp'

instance Num r ⇒ Default (Disk r) where
  def = Disk 0 0

instance (Floating r, RealFrac r, UV.Unbox r) ⇒ Simplex2D (Disk r) where
  {-# SPECIALISE instance Simplex2D (Disk Float ) #-}
  {-# SPECIALISE instance Simplex2D (Disk Double) #-}
  type BaseType (Disk r) = r
  accumulate v attn extrp _ _ gi_sph2 _ _
    = let attnSq = attn ⋅ attn
          extrp' = attnSq ⋅ attnSq ⋅ extrp
      in
        v & diskX +~ extrp' ⋅ (gradientsSph2 GV.! gi_sph2     )
          & diskY +~ extrp' ⋅ (gradientsSph2 GV.! (gi_sph2 + 1))

instance Num r ⇒ Default (Derivative r) where
  def = Derivative 0 0

instance (Floating r, RealFrac r, UV.Unbox r) ⇒ Simplex2D (Derivative r) where
  {-# SPECIALISE instance Simplex2D (Derivative Float ) #-}
  {-# SPECIALISE instance Simplex2D (Derivative Double) #-}
  type BaseType (Derivative r) = r
  accumulate v attn extrp gx gy _ dx' dy'
    = let attnSq = attn ⋅ attn
      in
        v & dx +~ (gx ⋅ attn - 8 ⋅ dx' ⋅ extrp) ⋅ attnSq ⋅ attn
          & dy +~ (gy ⋅ attn - 8 ⋅ dy' ⋅ extrp) ⋅ attnSq ⋅ attn

-- 2D lattice lookup table (KdotJPG)
lookup2D ∷ (Floating r, UV.Unbox r) ⇒ UV.Vector (LatticePoint2D r)
{-# SPECIALISE NOINLINE lookup2D ∷ UV.Vector (LatticePoint2D Float ) #-}
{-# SPECIALISE NOINLINE lookup2D ∷ UV.Vector (LatticePoint2D Double) #-}
lookup2D = runST $
           do mv ← GMV.unsafeNew (8 * 4)
              for_ [0 .. 7] $ \i →
                let (i1, j1, i2, j2)
                      = if (i .&. 1) ≡ 0
                        then
                          let (i1', j1') = if (i .&. 2) ≡ 0
                                           then (0,  0)
                                           else (2,  0)
                              (i2', j2') = if (i .&. 4) ≡ 0
                                           then (1, -1)
                                           else (1,  1)
                          in (i1', j1', i2', j2')
                        else
                          let (i1', j1') = if (i .&. 2) ≡ 0
                                           then (-1, 1)
                                           else ( 1, 1)
                              (i2', j2') = if (i .&. 4) ≡ 0
                                           then ( 0, 0)
                                           else ( 0, 2)
                          in (i1', j1', i2', j2')
                in
                  do GMV.write mv (i ⋅ 4    ) (latticePoint2D  1  0)
                     GMV.write mv (i ⋅ 4 + 1) (latticePoint2D  0  1)
                     GMV.write mv (i ⋅ 4 + 2) (latticePoint2D i1 j1)
                     GMV.write mv (i ⋅ 4 + 3) (latticePoint2D i2 j2)
              GV.unsafeFreeze mv

-- 3D contribution lookup table (DigitalShadow)
lookup3D ∷ ∀r. ( Floating r
               , GMV.MVector BV.MVector (Contribution3D r)
               )
         ⇒ BV.Vector (Contribution3D r)
{-# SPECIALISE NOINLINE lookup3D ∷ BV.Vector (Contribution3D Float ) #-}
{-# SPECIALISE NOINLINE lookup3D ∷ BV.Vector (Contribution3D Double) #-}
lookup3D = runST $
           do mv ← GMV.unsafeNew 2048
              for_ [0, 2 .. (GV.length lookupPairs3D) - 1] $ \i →
                GMV.write mv (lookupPairs3D GV.! i)
                             (contributions3D GV.! (lookupPairs3D GV.! (i + 1)))
              GV.unsafeFreeze mv
  where
    base3D ∷ BV.Vector (UV.Vector Int)
    {-# NOINLINE base3D #-}
    base3D =
      GV.fromList
      [ GV.fromList [0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1]
      , GV.fromList [2, 1, 1, 0, 2, 1, 0, 1, 2, 0, 1, 1, 3, 1, 1, 1]
      , GV.fromList [1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 2, 1, 1, 0, 2, 1, 0, 1, 2, 0, 1, 1]
      ]

    p3D ∷ UV.Vector Int
    {-# NOINLINE p3D #-}
    p3D =
      GV.fromList
      [  0,  0,  1, -1,  0,  0,  1,  0, -1,  0,  0, -1
      ,  1,  0,  0,  0,  1, -1,  0,  0, -1,  0,  1,  0
      ,  0, -1,  1,  0,  2,  1,  1,  0,  1,  1,  1, -1
      ,  0,  2,  1,  0,  1,  1,  1, -1,  1,  0,  2,  0
      ,  1,  1,  1, -1,  1,  1,  1,  3,  2,  1,  0,  3
      ,  1,  2,  0,  1,  3,  2,  0,  1,  3,  1,  0,  2
      ,  1,  3,  0,  2,  1,  3,  0,  1,  2,  1,  1,  1
      ,  0,  0,  2,  2,  0,  0,  1,  1,  0,  1,  0,  2
      ,  0,  2,  0,  1,  1,  0,  0,  1,  2,  0,  0,  2
      ,  2,  0,  0,  0,  0,  1,  1, -1,  1,  2,  0,  0
      ,  0,  0,  1, -1,  1,  1,  2,  0,  0,  0,  0,  1
      ,  1,  1, -1,  2,  3,  1,  1,  1,  2,  0,  0,  2
      ,  2,  3,  1,  1,  1,  2,  2,  0,  0,  2,  3,  1
      ,  1,  1,  2,  0,  2,  0,  2,  1,  1, -1,  1,  2
      ,  0,  0,  2,  2,  1,  1, -1,  1,  2,  2,  0,  0
      ,  2,  1, -1,  1,  1,  2,  0,  0,  2,  2,  1, -1
      ,  1,  1,  2,  0,  2,  0,  2,  1,  1,  1, -1,  2
      ,  2,  0,  0,  2,  1,  1,  1, -1,  2,  0,  2,  0 ]

    lookupPairs3D ∷ UV.Vector Int
    {-# NOINLINE lookupPairs3D #-}
    lookupPairs3D
      = GV.fromList
        [    0,  2,    1,  1,    2,  2,    5,  1
        ,    6,  0,    7,  0,   32,  2,   34,  2
        ,  129,  1,  133,  1,  160,  5,  161,  5
        ,  518,  0,  519,  0,  546,  4,  550,  4
        ,  645,  3,  647,  3,  672,  5,  673,  5
        ,  674,  4,  677,  3,  678,  4,  679,  3
        ,  680, 13,  681, 13,  682, 12,  685, 14
        ,  686, 12,  687, 14,  712, 20,  714, 18
        ,  809, 21,  813, 23,  840, 20,  841, 21
        , 1198, 19, 1199, 22, 1226, 18, 1230, 19
        , 1325, 23, 1327, 22, 1352, 15, 1353, 17
        , 1354, 15, 1357, 17, 1358, 16, 1359, 16
        , 1360, 11, 1361, 10, 1362, 11, 1365, 10
        , 1366,  9, 1367,  9, 1392, 11, 1394, 11
        , 1489, 10, 1493, 10, 1520,  8, 1521,  8
        , 1878,  9, 1879,  9, 1906,  7, 1910,  7
        , 2005,  6, 2007,  6, 2032,  8, 2033,  8
        , 2034,  7, 2037,  6, 2038,  7, 2039,  6 ]

    contributions3D ∷ BV.Vector (Contribution3D r)
    {-# NOINLINE contributions3D #-}
    contributions3D
      = runST $
        do mv ← GMV.unsafeNew (GV.length p3D `div` 9)
                ∷ ST σ (BV.MVector σ (MContribution3D σ r))
           for_ [0, 9 .. (GV.length p3D) - 1] $ \i →
             do let baseSet = base3D GV.! (p3D GV.! i)
                previous ← newSTRef Nothing
                current  ← newSTRef Nothing
                for_ [0, 4 .. (GV.length baseSet) - 1] $ \j →
                  do curr ← contribution3D' (fromIntegral (baseSet GV.! j))
                                            (baseSet GV.! (j + 1))
                                            (baseSet GV.! (j + 2))
                                            (baseSet GV.! (j + 3))
                     writeSTRef current (Just curr)
                     prev' ← readSTRef previous
                     case prev' of
                       Nothing   → GMV.write mv (i `div` 9) curr
                       Just prev → writeSTRef (prev^.c'next) (Just curr)
                     writeSTRef previous (Just curr)
                curr' ← readSTRef current
                case curr' of
                  Nothing   → return ()
                  Just curr →
                    do writeSTRef (curr^.c'next) =<<
                         Just <$> contribution3D' (fromIntegral (p3D GV.! (i + 1)))
                                                  (p3D GV.! (i + 2))
                                                  (p3D GV.! (i + 3))
                                                  (p3D GV.! (i + 4))
                       Just next ← readSTRef (curr^.c'next)
                       writeSTRef (next^.c'next) =<<
                         Just <$> contribution3D' (fromIntegral (p3D GV.! (i + 5)))
                                                  (p3D GV.! (i + 6))
                                                  (p3D GV.! (i + 7))
                                                  (p3D GV.! (i + 8))
           GV.generateM (GMV.length mv) $ \i →
             do cont ← GMV.read mv i
                contribution3D cont

-- | Construct an OpenSimplex noise generator with a given 64-bits
-- seed. Since this is quite an expensive operation, one should reuse
-- generators as far as possible.
mkSimplexGen ∷ Int64 → SimplexGen
mkSimplexGen seed
  = runST $
    do source ← GMV.unsafeNew 1024 ∷ ST σ (UV.MVector σ Int)
       for_ [0 .. 1023] $ \i → GMV.write source i i

       perm       ← GMV.unsafeNew 1024
       perm2D     ← GMV.unsafeNew 1024
       perm2DSph2 ← GMV.unsafeNew 1024
       perm3D     ← GMV.unsafeNew 1024

       let go i seed'
             | i < 0 = return ()
             | otherwise
                 = do let seed'' = seed' ⋅ 6364136223846793005 + 1442695040888963407
                          r0     = fromIntegral $ (seed + 31) `rem` fromIntegral (i + 1)
                          r      = if r0 < 0 then r0 + i + 1 else r0

                      perm_i ← GMV.read source r
                      GMV.write perm       i perm_i
                      GMV.write perm2D     i ((perm_i `rem` 12) ⋅ 2)
                      GMV.write perm2DSph2 i (((perm_i `div` 12) `rem` 12) ⋅ 2)
                      GMV.write perm3D     i ((perm_i `rem` 48) ⋅ 3)

                      GMV.write source r =<< GMV.read source i
                      go (i-1) seed''
       go 1023 seed

       perm'       ← GV.unsafeFreeze perm
       perm2D'     ← GV.unsafeFreeze perm2D
       perm2DSph2' ← GV.unsafeFreeze perm2DSph2
       perm3D'     ← GV.unsafeFreeze perm3D
       return SimplexGen
         { _sgPerm       = perm'
         , _sgPerm2D     = perm2D'
         , _sgPerm2DSph2 = perm2DSph2'
         , _sgPerm3D     = perm3D'
         }

floorAndAbsFrac ∷ RealFrac r ⇒ r → (Int, r)
{-# SPECIALISE floorAndAbsFrac ∷ Float  → (Int, Float ) #-}
{-# SPECIALISE floorAndAbsFrac ∷ Double → (Int, Double) #-}
floorAndAbsFrac r
  = let (n, f) = properFraction r
    in if r < 0
       then (n-1, abs f)
       else (n  , abs f)

-- | 2D OpenSimplex noise (KdotJPG)
simplex2D ∷ (Simplex2D α, BaseType α ~ r)
          ⇒ SimplexGen
          → r -- ^ x
          → r -- ^ y
          → α
{-# SPECIALISE simplex2D ∷ SimplexGen → Float  → Float  → Scalar     Float  #-}
{-# SPECIALISE simplex2D ∷ SimplexGen → Float  → Float  → Disk       Float  #-}
{-# SPECIALISE simplex2D ∷ SimplexGen → Float  → Float  → Derivative Float  #-}
{-# SPECIALISE simplex2D ∷ SimplexGen → Double → Double → Scalar     Double #-}
{-# SPECIALISE simplex2D ∷ SimplexGen → Double → Double → Disk       Double #-}
{-# SPECIALISE simplex2D ∷ SimplexGen → Double → Double → Derivative Double #-}
simplex2D gen x y
  =     -- Get points for A2 lattice
    let s  = stretch2D ⋅ (x + y)
        xs = x + s
        ys = y + s
        -- Get base points and offsets
        (xsb, xsi) = floorAndAbsFrac xs
        (ysb, ysi) = floorAndAbsFrac ys
        -- Index to point list
        index = let a = truncate (ysi - xsi + 1)
                in (a `shiftL` 2) .|.
                   (truncate (xsi + ysi / 2 + (fromIntegral a) / 2) `shiftL` 3) .|.
                   (truncate (ysi + xsi / 2 + 1 / 2 - (fromIntegral a) / 2) `shiftL` 4)
        -- Get unskewed offsets.
        ssi = (xsi + ysi) ⋅ squish2D
        xi  = xsi + ssi
        yi  = ysi + ssi
        -- Point contributions
        accum i res
          | i < 4
              = let lattice = lookup2D GV.! (index + i)

                    δx   = xi + lattice^.lδx
                    δy   = yi + lattice^.lδy
                    attn = 2 - (δx ⋅ δx) - (δy ⋅ δy)
                in
                  if attn ≤ 0
                  then accum (i+1) res
                  else
                    let pxm     = (xsb + lattice^.lxsv) .&. 1023
                        pym     = (ysb + lattice^.lysv) .&. 1023
                        gi_p    = ((gen^.sgPerm) GV.! pxm) `xor` pym
                        gi      = (gen^.sgPerm2D) GV.! gi_p
                        gx      = gradients2D GV.! gi
                        gy      = gradients2D GV.! (gi + 1)
                        extrp   = gx ⋅ δx + gy ⋅ δy
                        gi_sph2 = (gen^.sgPerm2DSph2) GV.! gi_p
                        res'    = accumulate res attn extrp gx gy gi_sph2 δx δy
                    in accum (i+1) res'
          | otherwise
              = res
    in
      accum 0 def

-- | 3D OpenSimplex Noise (DigitalShadow)
simplex3D ∷ (Floating r, RealFrac r, UV.Unbox r)
          ⇒ SimplexGen
          → r -- ^ x
          → r -- ^ y
          → r -- ^ z
          → r
{-# SPECIALISE simplex3D ∷ SimplexGen → Float  → Float  → Float  → Float  #-}
{-# SPECIALISE simplex3D ∷ SimplexGen → Double → Double → Double → Double #-}
simplex3D gen x y z
  = let stretchOffset = (x + y + z) ⋅ stretch3D
        xs = x + stretchOffset
        ys = y + stretchOffset
        zs = z + stretchOffset

        (xsb, xins) = floorAndAbsFrac xs
        (ysb, yins) = floorAndAbsFrac ys
        (zsb, zins) = floorAndAbsFrac zs

        squishOffset = fromIntegral (xsb + ysb + zsb) ⋅ squish3D
        δx0 = x - (fromIntegral xsb + squishOffset)
        δy0 = y - (fromIntegral ysb + squishOffset)
        δz0 = z - (fromIntegral zsb + squishOffset)

        inSum = xins + yins + zins

        hash = ( truncate (yins - zins + 1)) .|.
               ((truncate (xins - yins + 1)) `shiftL` 1) .|.
               ((truncate (xins - zins + 1)) `shiftL` 2) .|.
               ((truncate inSum)             `shiftL` 3) .|.
               ((truncate (inSum + zins))    `shiftL` 5) .|.
               ((truncate (inSum + yins))    `shiftL` 7) .|.
               ((truncate (inSum + xins))    `shiftL` 9)

        c0 = lookup3D GV.! hash

        accum Nothing  res = res
        accum (Just c) res
          = let δx = δx0 + c^.cδx
                δy = δy0 + c^.cδy
                δz = δz0 + c^.cδz

                attn = 2 - δx ⋅ δx - δy ⋅ δy - δz ⋅ δz
            in
              if attn ≤ 0
              then accum (c^.cnext) res
              else
                let px = xsb + c^.cxsb
                    py = ysb + c^.cysb
                    pz = zsb + c^.czsb

                    i_x = (gen^.sgPerm)   GV.! (           px  .&. 0x3FF)
                    i_y = (gen^.sgPerm)   GV.! ((i_x `xor` py) .&. 0x3FF)
                    i   = (gen^.sgPerm3D) GV.! ((i_y `xor` pz) .&. 0x3FF)

                    valuePart = (gradients3D GV.!  i     ) ⋅ δx +
                                (gradients3D GV.! (i + 1)) ⋅ δy +
                                (gradients3D GV.! (i + 1)) ⋅ δz

                    attnSq = attn ⋅ attn
                in
                  accum (c^.cnext) (res + attnSq ⋅ attnSq ⋅ valuePart)
    in
      accum (Just c0) 0
