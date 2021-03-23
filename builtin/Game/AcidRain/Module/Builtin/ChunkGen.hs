{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen
  ( terraform
  ) where

import Control.Eff (Eff, Lifted, Member)
import Control.Eff.Reader.Lazy (Reader)
import Data.Foldable (for_)
import Game.AcidRain.Module.Builtin.ChunkGen.SpaceAttrs
  ( remappedHeight, isWaterLogged, spaceAttrs )
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo (WorldInfo(..))
import Game.AcidRain.World.Chunk (chunkSize, chunkHeight)
import Game.AcidRain.World.Chunk.Generator (ChunkGenM, chunkPos)
import Game.AcidRain.World.Chunk.Position (toWorldPos)
import Game.AcidRain.World.Position (wpX, wpY, lowestZ)
import Lens.Micro ((&), (+~))


-- | The built-in terrain generator. Internally it creates a height
-- map where 63.0 is the sea level, and then brutally remaps it in
-- @[-1, 0]@ because our actual world is only 2 tiles high. Since the
-- z position 0 is the highest position in the world, and players
-- cannot pass through solid tiles at the position, this terrain
-- generator applies a bias not to generate too many tiles at @z = 0@.
terraform ∷ (Member (Reader WorldInfo) r, Lifted ChunkGenM r) ⇒ Eff r ()
terraform
  = do cPos ← chunkPos
       --air  ← 
       for_ [0, chunkSize-1] $ \y →
         for_ [0, chunkSize-1] $ \x →
           do let wPos = (toWorldPos cPos 0) & wpX +~ x
                                             & wpY +~ y
              attrs ← spaceAttrs wPos
              let height = remappedHeight attrs
              for_ [lowestZ, lowestZ+chunkHeight-1] $ \z →
                error "FIXME"
       return ()
