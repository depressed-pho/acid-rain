{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen
  ( terraform
  ) where

import Game.AcidRain.World.Chunk.Generator (ChunkGenM)


terraform ∷ ChunkGenM ()
terraform = return ()
