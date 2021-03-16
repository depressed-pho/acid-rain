{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk.Position
  ( ChunkPos(..), cpX, cpY
  , toWorldPos
  ) where

import Data.Convertible.Base (Convertible(..))
import Data.Hashable (Hashable)
import Data.Int (Int8, Int32)
import Game.AcidRain.World.Position (WorldPos(..), wpX, wpY)
import Game.AcidRain.World.Chunk (chunkSize)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((⋅))


-- | A point representing a chunk position in (x, y) coordinate
-- space. It is computed by dividing 'WorldPos' by 'chunkSize' and
-- rounding towards negative infinity.
data ChunkPos = ChunkPos
  { _cpX ∷ {-# UNPACK #-} !Int32
  , _cpY ∷ {-# UNPACK #-} !Int32
  } deriving (Eq, Show, Generic)

makeLenses ''ChunkPos

instance Hashable ChunkPos

instance Convertible WorldPos ChunkPos where
  safeConvert wp
    = Right $ ChunkPos
      { _cpX = wp^.wpX `div` chunkSize
      , _cpY = wp^.wpY `div` chunkSize
      }

-- | There is no instance of @'Convertible' 'WorldPos' 'ChunkPos'@
-- because 'ChunkPos' has no @z@ component.
toWorldPos ∷ ChunkPos → Int8 → WorldPos
toWorldPos cp z
  = WorldPos
    { _wpX = cp^.cpX ⋅ chunkSize
    , _wpY = cp^.cpY ⋅ chunkSize
    , _wpZ = z
    }
