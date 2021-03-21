{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | This is an internal module used by
-- 'Game.AcidRain.World.Local.LocalWorld' to delegate chunk
-- management. Definitely not for user consumption.
module Game.AcidRain.World.Chunk.Manager.Local
  ( LocalChunkManager
  , new
  , lookup
  , get
  , modify

  , generate
  ) where

import Control.Concurrent (forkIO)
import Control.Eff (Eff, Lifted, Member, runLift, lift)
import Control.Eff.Exception (Exc, runError)
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.STM (STM, atomically, retry, throwSTM)
import qualified Focus as F
import GHC.Conc (unsafeIOToSTM)
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Generator (ChunkGenerator, generateChunk)
import Game.AcidRain.World.Chunk.Palette (TilePalette)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import Prelude hiding (lcm, lookup)
import Prelude.Unicode ((∘))
import StmContainers.Map (Map)
import qualified StmContainers.Map as SM


data ChunkCell
  = Loaded !Chunk
  | LoadFailed !SomeException

evalCell ∷ ChunkCell → STM Chunk
evalCell (Loaded c    ) = return c
evalCell (LoadFailed e) = throwSTM e

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
    , lcmChunkGen ∷ !ChunkGenerator
      -- | Note that this is an STM map. Accessing it requires an STM
      -- transaction.
    , lcmCells    ∷ !(Map ChunkPos ChunkCell)
    }

instance Show LocalChunkManager where
  showsPrec d lcm
    = showParen (d > appPrec) $
      showString "LocalChunkManager " ∘
      showString "{ lcmTiles = "    ∘ showsPrec (appPrec + 1) (lcmTiles    lcm) ∘
      showString ", lcmPalette = "  ∘ showsPrec (appPrec + 1) (lcmPalette  lcm) ∘
      showString ", lcmEntities = " ∘ showsPrec (appPrec + 1) (lcmEntities lcm) ∘
      showString ", .. }"
    where
      appPrec = 10

-- | Construct a new chunk manager out of a tile registry, and a
-- properly populated tile palette and catalogues. Care must be taken
-- because if the palette is somehow invalid it will later result in
-- exceptions (or even chunk corruptions!) but not immediately.
new ∷ TileRegistry → TilePalette → EntityCatalogue → ChunkGenerator → STM LocalChunkManager
new tReg tPal eCat cGen
  = do cells ← SM.new
       return $ LocalChunkManager
         { lcmTiles    = tReg
         , lcmPalette  = tPal
         , lcmEntities = eCat
         , lcmChunkGen = cGen
         , lcmCells    = cells
         }

-- | Get the chunk at a certain position if it's available. This has
-- no side effects.
lookup ∷ ChunkPos → LocalChunkManager → STM (Maybe Chunk)
lookup pos lcm
  -- Smells like a point-free opportunity, but I don't like unreadable
  -- code.
  = mapM evalCell =<< (SM.lookup pos $ lcmCells lcm)

-- | Get the chunk at a certain position. If it's
-- not already loaded, a chunk will be loaded or generated on a
-- separate thread and the transaction will be retried.
get ∷ ChunkPos → LocalChunkManager → STM Chunk
get pos lcm
  -- Loading chunks involves I/O, but we can't safely do that in an
  -- STM transaction. So we do it outside of a transaction and discard
  -- it if a race condition happens.
  = do chunk' ← SM.lookup pos (lcmCells lcm)
       case chunk' of
         Just cell → evalCell cell
         Nothing →
           do void $ unsafeIOToSTM $ forkIO $ runLift $
                do cell ← either LoadFailed Loaded <$> runError loadOrGenerate
                   lift $ atomically $ SM.focus (focIns cell) pos (lcmCells lcm)
              retry
  where
    loadOrGenerate ∷ (Member (Exc SomeException) r, Lifted IO r) ⇒ Eff r Chunk
    loadOrGenerate = generate pos lcm -- FIXME: load

    focIns ∷ ChunkCell → F.Focus ChunkCell STM ()
    focIns cell
      = do raced ← F.member
           if raced
             then return ()
             else F.insert cell

-- | Apply a modification to the chunk at a certain position. If it's
-- not already loaded, a chunk will be loaded or generated on a
-- separate thread and the transaction will be retried.
modify ∷ (Chunk → Chunk) → ChunkPos → LocalChunkManager → STM ()
modify f pos lcm
  = do chunk ← get pos lcm
       SM.insert (Loaded (f chunk)) pos (lcmCells lcm)

-- | Generate a chunk. Since chunk generation is a time consuming
-- task, callers are highly advised to do it in a separate thread.
generate ∷ (Member (Exc SomeException) r, Lifted IO r)
         ⇒ ChunkPos
         → LocalChunkManager
         → Eff r Chunk
generate cPos (LocalChunkManager { .. })
  = generateChunk lcmTiles lcmPalette lcmEntities lcmChunkGen cPos
