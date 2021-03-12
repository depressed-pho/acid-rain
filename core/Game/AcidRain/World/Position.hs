{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Position
  ( WorldPos(..)
  ) where

import Data.Int (Int8, Int32)


-- | A point representing a world position in (x, y, z) coordinate
-- space.
data WorldPos = WorldPos
  { x ∷ {-# UNPACK #-} !Int32
  , y ∷ {-# UNPACK #-} !Int32
    -- | The z coordinate is either -1 or 0.
  , z ∷ {-# UNPACK #-} !Int8
  } deriving (Eq, Show)
