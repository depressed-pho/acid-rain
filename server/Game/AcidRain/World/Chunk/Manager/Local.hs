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
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Game.AcidRain.World.Chunk (Chunk)
import qualified Game.AcidRain.World.Chunk as C
import Game.AcidRain.World.Chunk.Palette (TilePalette)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Tile (defaultState)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Prelude hiding (lcm, lookup)


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
    { lcmTiles   ∷ !TileRegistry
    , lcmPalette ∷ !TilePalette
    , lcmLoaded  ∷ !(TVar (HashMap ChunkPos Chunk))
    }

-- | Construct a new chunk manager out of a tile registry and a
-- properly populated tile palette. Care must be taken because if the
-- palette is somehow invalid it will later result in exceptions (or
-- even chunk corruptions!) but not immediately.
new ∷ TileRegistry → TilePalette → STM LocalChunkManager
new tiles palette
  = do loaded ← newTVar HM.empty
       return $ LocalChunkManager
         { lcmTiles   = tiles
         , lcmPalette = palette
         , lcmLoaded  = loaded
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
  -- Smells like a point-free opportunity, but I also don't like
  -- unreadable code.
  = do chunks ← readTVar $ lcmLoaded lcm
       return $ HM.lookup pos chunks

-- FIXME: Remove this later.
ensureChunkExists ∷ ChunkPos → LocalChunkManager → STM ()
ensureChunkExists pos lcm
  = do chunks ← readTVar $ lcmLoaded lcm
       if HM.member pos chunks
         then return ()
         else do chunk ← generate pos lcm
                 writeTVar (lcmLoaded lcm) $ HM.insert pos chunk chunks
