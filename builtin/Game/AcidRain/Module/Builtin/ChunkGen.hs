{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen
  ( terraform
  ) where

import Control.Eff (Eff, Lifted, Member)
import Control.Eff.State.Strict (State)
import Game.AcidRain.World.Chunk.Generator (ChunkGenState)


terraform ∷ (Member (State ChunkGenState) r, Lifted IO r) ⇒ Eff r ()
terraform = return ()
