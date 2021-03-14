{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Default.Class (def)
import Game.AcidRain.Module (Module(..))
import Game.AcidRain.Module.Builtin (BuiltinModule)
import Game.AcidRain.World (World(..), WorldMode(..))
import Game.AcidRain.World.Local (newWorld)

main ∷ IO ()
main
  = do lw ← newWorld SinglePlayer [upcastModule $ def @BuiltinModule]
       forever $
         do st ← getWorldState lw
            putStrLn (show st)
            threadDelay 1000000
