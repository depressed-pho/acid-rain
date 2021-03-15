{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Control.Monad (forever)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module (Module(..))
import Game.AcidRain.Module.Builtin (BuiltinModule)
import Game.AcidRain.World (World(..), WorldMode(..))
import Game.AcidRain.World.Chunk.Position (ChunkPos(..))
import Game.AcidRain.World.Local (newWorld)

main ∷ IO ()
main
  = do lw ← newWorld SinglePlayer [upcastModule (Proxy ∷ Proxy BuiltinModule)]
       forever $
         do e ← waitForEvent lw
            putStrLn (show e)
            ensureChunkExists lw (ChunkPos 0 0)
