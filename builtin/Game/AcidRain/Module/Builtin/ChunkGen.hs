{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen
  ( terraform
  ) where

import Control.Eff (Eff)
import Game.AcidRain.World.Chunk.Generator (ChunkGen)


terraform ∷ Eff '[ChunkGen] ()
terraform = return ()
