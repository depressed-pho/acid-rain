{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Position
  ( WorldPos(..), wpX, wpY, wpZ
  , lowestZ
  ) where

import Data.Int (Int8, Int32)
import Lens.Micro.TH (makeLenses)

-- | A point representing a world position in (x, y, z) coordinate
-- space.
data WorldPos = WorldPos
  { _wpX ∷ {-# UNPACK #-} !Int32
  , _wpY ∷ {-# UNPACK #-} !Int32
    -- | The z coordinate is either -1 or 0.
  , _wpZ ∷ {-# UNPACK #-} !Int8
  } deriving (Eq, Show)

lowestZ ∷ Int8
lowestZ = -1

makeLenses ''WorldPos
