{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | This is an internal module used by
-- 'Game.AcidRain.World.Local.LocalWorld' to delegate chunk
-- management. Definitely not for user consumption.
module Game.AcidRain.World.Chunk.Manager.Local
  ( LocalChunkManager
  , new
  , lookup
  , ensureChunkExists
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.STM (STM)
import Game.AcidRain.World.Chunk (Chunk)
import qualified Game.AcidRain.World.Chunk as C
import Game.AcidRain.World.Chunk.Palette (TilePalette)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue)
import Game.AcidRain.World.Tile (defaultState)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Prelude hiding (lcm, lookup)
import Prelude.Unicode ((∘))
import StmContainers.Map (Map)
import qualified StmContainers.Map as SM


-- | This is a server-side chunk manager. When a chunk is requested, it
-- searches for the chunk in the loaded chunk map, or loads the chunk
-- from disk, or performs a chunk generation. The server thread
-- periodically asks the manager to tick and save chunks.
--
-- If a chunk has no players nearby, and is not anchored by any tile
-- entities, the chunk will be unloaded from memory. The cache is
-- therefore not an LRU.
data LocalChunkManager
  = LocalChunkManager
    { lcmTiles    ∷ !TileRegistry
    , lcmPalette  ∷ !TilePalette
    , lcmEntities ∷ !EntityCatalogue
      -- | Note that this is an STM map. Accessing it requires an STM
      -- transaction.
    , lcmLoaded  ∷ !(Map ChunkPos Chunk)
    }

instance Show LocalChunkManager where
  showsPrec d lcm
    = showParen (d > appPrec) $
      showString "LocalChunkManager " ∘
      showString "{ lcmTiles = " ∘ showsPrec (appPrec + 1) (lcmTiles lcm) ∘
      showString ", lcmPalette = " ∘ showsPrec (appPrec + 1) (lcmPalette lcm) ∘
      showString ", lcmEntities = " ∘ showsPrec (appPrec + 1) (lcmEntities lcm) ∘
      showString ", .. }"
    where
      appPrec = 10

-- | Construct a new chunk manager out of a tile registry, and a
-- properly populated tile palette and catalogues. Care must be taken
-- because if the palette is somehow invalid it will later result in
-- exceptions (or even chunk corruptions!) but not immediately.
new ∷ TileRegistry → TilePalette → EntityCatalogue → STM LocalChunkManager
new tReg tPal eCat
  = do loaded ← SM.new
       return $ LocalChunkManager
         { lcmTiles    = tReg
         , lcmPalette  = tPal
         , lcmEntities = eCat
         , lcmLoaded   = loaded
         }

-- | Generate a chunk. Since chunk generation is a time consuming
-- task, callers are highly advised to do it in a separate thread.
generate ∷ MonadThrow μ ⇒ ChunkPos → LocalChunkManager → μ Chunk
generate _pos lcm
  -- FIXME: Do it properly.
  = do let reg = lcmTiles lcm
       dirt ← TR.get "acid-rain:dirt" reg
       C.new reg (lcmPalette lcm) (defaultState dirt)

-- | Get the chunk at a certain position if it's available. This has
-- no side effects.
lookup ∷ ChunkPos → LocalChunkManager → STM (Maybe Chunk)
lookup pos lcm
  -- Smells like a point-free opportunity, but I don't like unreadable
  -- code.
  = SM.lookup pos $ lcmLoaded lcm

-- FIXME: Remove this later.
ensureChunkExists ∷ ChunkPos → LocalChunkManager → STM ()
ensureChunkExists pos lcm
  = do chunk' ← SM.lookup pos $ lcmLoaded lcm
       case chunk' of
         Just _  → return ()
         Nothing →
           do chunk ← generate pos lcm
              SM.insert chunk pos $ lcmLoaded lcm
