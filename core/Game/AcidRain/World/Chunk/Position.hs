{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk.Position
  ( ChunkPos(..)
  , toChunkPos
  ) where

import Data.Convertible.Base (Convertible(..))
import Data.Hashable (Hashable)
import Data.Int (Int8, Int32)
import Game.AcidRain.World.Position (WorldPos(..))
import Game.AcidRain.World.Chunk (chunkSize)
import GHC.Generics (Generic)
import Prelude.Unicode ((⋅))


-- | A point representing a chunk position in (x, y) coordinate
-- space. It is computed by dividing 'WorldPos' by 'chunkSize' and
-- rounding towards negative infinity.
data ChunkPos = ChunkPos
  { x ∷ {-# UNPACK #-} !Int32
  , y ∷ {-# UNPACK #-} !Int32
  } deriving (Eq, Show, Generic)

instance Hashable ChunkPos

instance Convertible WorldPos ChunkPos where
  safeConvert (WorldPos { x, y, .. })
    = Right $ ChunkPos
      { x = x `mod` chunkSize
      , y = y `mod` chunkSize
      }

-- | There is no instance of @'Convertible' 'WorldPos' 'ChunkPos'@
-- because 'ChunkPos' has no @z@ component.
toChunkPos ∷ Int8 → ChunkPos → WorldPos
toChunkPos z (ChunkPos { x, y })
  = WorldPos
    { x = x ⋅ chunkSize
    , y = y ⋅ chunkSize
    , z = z
    }
