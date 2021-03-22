{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen
  ( terraform
  ) where

import Control.Eff (Eff, Lifted, Member)
import Control.Eff.Reader.Lazy (Reader)
import Game.AcidRain.Module.Builtin.ChunkGen.SpaceAttr (spaceAttr)
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo (WorldInfo(..))
import Game.AcidRain.World.Chunk.Generator (ChunkGenM, chunkPos)
import Game.AcidRain.World.Chunk.Position (toWorldPos)


-- | The built-in terrain generator. Internally it creates a height
-- map where 63.0 is the sea level, and then brutally remaps it in
-- @[-1, 0]@ because our actual world is only 2 tiles high. Since the
-- z position 0 is the highest position in the world, and players
-- cannot pass through solid tiles at the position, this terrain
-- generator applies a bias not to generate too many tiles at @z = 0@.
terraform ∷ (Member (Reader WorldInfo) r, Lifted ChunkGenM r) ⇒ Eff r ()
terraform
  = do cPos ← chunkPos
       attr ← spaceAttr (toWorldPos cPos 0)
       error (show attr)
       return ()
