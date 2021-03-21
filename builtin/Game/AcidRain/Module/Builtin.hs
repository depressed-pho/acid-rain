{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin
  ( BuiltinModule
  ) where

import Data.Proxy (Proxy)
import qualified Game.AcidRain.Module.Builtin.ChunkGen as CG
import Game.AcidRain.Module (Module(..))
import Game.AcidRain.Module.Builtin.Loader.Entity (loadEntities)
import Game.AcidRain.Module.Builtin.Loader.Tile (loadTiles)
import Game.AcidRain.Module.Loader (modifyChunkGenerator)
import Game.AcidRain.World.Chunk.Generator (terraform)
import Lens.Micro ((%~))


-- | This is a built-in module of Acid Rain whose existence is
-- mandatory because it's used by the base game logic.
data BuiltinModule

instance Module (Proxy BuiltinModule) where
  modID _ = "acid-rain"
  load _
    = do loadTiles
         loadEntities
         modifyChunkGenerator $
           terraform %~ (>> CG.terraform)
