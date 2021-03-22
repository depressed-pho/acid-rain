{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen
  ( terraform
  ) where

import Control.Eff (Eff, Lifted, Member)
import Control.Eff.Reader.Lazy (Reader)
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo (WorldInfo)
import Game.AcidRain.World.Chunk.Generator (ChunkGenM)


terraform ∷ (Member (Reader WorldInfo) r, Lifted ChunkGenM r) ⇒ Eff r ()
terraform = return ()
