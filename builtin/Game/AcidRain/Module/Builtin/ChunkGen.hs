{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.ChunkGen
  ( terraform
  ) where

import Control.Eff (Eff, Lifted, Member)
import Control.Eff.Reader.Lazy (Reader)
import Data.Convertible.Base (convert)
import Data.Foldable (for_)
import Game.AcidRain.Module.Builtin.ChunkGen.PointAttrs
  ( PointAttrs(..), remappedHeight, pointAttrs )
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo (WorldInfo(..))
import Game.AcidRain.World.Chunk (chunkSize, chunkHeight)
import Game.AcidRain.World.Chunk.Generator
  ( ChunkGenM, chunkPos, putClimate )
import Game.AcidRain.World.Chunk.Position (toWorldPos)
import Game.AcidRain.World.Position (wpX, wpY, wpZ, lowestZ)
import Lens.Micro ((&), (.~), (+~))


-- | The built-in terrain generator. Internally it creates a height
-- map in @[-1, 1]@ where 0 is the sea level, and then remaps it in
-- integer @[-1, 0]@ because our actual world is only 2 tiles
-- high. Since the z position 0 is the highest position in the world,
-- and players cannot pass through solid tiles at the position, this
-- terrain generator applies a bias not to generate too many tiles at
-- @z = 0@.
terraform ∷ (Member (Reader WorldInfo) r, Lifted ChunkGenM r) ⇒ Eff r ()
terraform
  = do cPos ← chunkPos
       --air  ← 
       for_ [0, chunkSize-1] $ \y →
         for_ [0, chunkSize-1] $ \x →
           do let wPos0 = (toWorldPos cPos 0) & wpX +~ x
                                              & wpY +~ y
                  off0  = convert wPos0

              attrs ← pointAttrs wPos0
              putClimate off0 (paClimate attrs)

              --let height = remappedHeight attrs

              for_ [lowestZ, lowestZ+chunkHeight-1] $ \z →
                do --let wPos = wPos0 & wpZ .~ z
                   --    off  = convert wPos
                   return ()
       return ()
