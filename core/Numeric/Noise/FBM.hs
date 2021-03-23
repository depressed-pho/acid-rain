{-# LANGUAGE UnicodeSyntax #-}
-- | This module implements fractional Brownian motion with a
-- user-supplied noise function and parameters. See also
-- <https://www.iquilezles.org/www/articles/fbm/fbm.htm> and
-- <http://sbgames.org/sbgames2018/files/papers/ComputacaoShort/188264.pdf>.
module Numeric.Noise.FBM
  ( fBm
  ) where

import Prelude hiding (sum)
import Prelude.Unicode ((≥), (⋅))


fBm ∷ (Num r, Integral i)
    ⇒ (r → v → v) -- ^ Scalar multiplication of the vector.
    → (v → r)     -- ^ Noise function which maps a vector to a scalar value.
    → i           -- ^ Number of octaves.
    → r           -- ^ Initial amplutide, typically 1.
    → r           -- ^ Gain, the multiplier for amplutide for each step. Typically 0.5.
    → r           -- ^ Initial frequency.
    → r           -- ^ Lacunarity, the multiplier for frequency for each step. Typically 2.
    → v           -- ^ Input vector, typically a point in 2D coordinates.
    → r
{-# INLINEABLE fBm #-}
{-# SPECIALISE fBm
    ∷ (Double → v → v) → (v → Double) → Int → Double → Double → Double → Double → v → Double #-}
fBm scale noise oct amp0 gain freq0 lac vec = go amp0 freq0 0 0
  where
    go amp freq i sum
      | i ≥ oct   = sum
      | otherwise = let amp'  = amp ⋅ gain
                        freq' = freq ⋅ lac
                        sum'  = sum + amp ⋅ noise (scale freq vec)
                    in
                      go amp' freq' (i + 1) sum'
