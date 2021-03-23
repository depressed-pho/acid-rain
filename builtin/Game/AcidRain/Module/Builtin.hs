{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin
  ( BuiltinModule
  ) where

import Control.Eff.Reader.Lazy (runReader)
import Data.Proxy (Proxy)
import qualified Game.AcidRain.Module.Builtin.ChunkGen as CG
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo (worldInfo)
import Game.AcidRain.Module (Module(..))
--import Game.AcidRain.Module.Builtin.Biomes (loadBiomes)
import Game.AcidRain.Module.Builtin.Entities (loadEntities)
import Game.AcidRain.Module.Builtin.Tiles (loadTiles)
import Game.AcidRain.Module.Loader (getWorldSeed, modifyChunkGenerator)
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
         wi ← worldInfo <$> getWorldSeed
         modifyChunkGenerator $
           terraform %~ (>> runReader wi CG.terraform)
