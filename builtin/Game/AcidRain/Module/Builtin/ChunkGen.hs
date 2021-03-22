{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen
  ( terraform
  ) where

import Control.Eff (Eff, Lifted)
import Game.AcidRain.World.Chunk.Generator (ChunkGenM)


terraform ∷ Lifted ChunkGenM r ⇒ Eff r ()
terraform = return ()
