{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | This is an internal module used by
-- 'Game.AcidRain.World.Local.LocalWorld' to delegate chunk
-- management. Definitely not for user consumption.
module Game.AcidRain.World.Chunk.Manager.Local
  ( LocalChunkManager
  , ChunkStatus(..)

  , new
  , lookup
  , get
  , put
  , modify

  , generate
  ) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.STM.TSem (TSem, newTSem, waitTSem, signalTSem)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.STM (STM, atomically, retry, throwSTM)
import qualified Focus as F
import GHC.Conc (unsafeIOToSTM)
import Game.AcidRain.World.Biome.Palette (BiomePalette)
import Game.AcidRain.World.Biome.Registry (BiomeRegistry)
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Generator (ChunkGenerator, generateChunk)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue)
import Game.AcidRain.World.Tile.Palette (TilePalette)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import Prelude hiding (lcm, lookup)
import Prelude.Unicode ((∘))
import StmContainers.Map (Map)
import qualified StmContainers.Map as SM


data ChunkStatus
  = Loading
  | Loaded !Chunk

type ChunkCell = Either SomeException ChunkStatus

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
    { lcmTileReg     ∷ !TileRegistry
    , lcmTilePal     ∷ !TilePalette
    , lcmBiomeReg    ∷ !BiomeRegistry
    , lcmBiomePal    ∷ !BiomePalette
    , lcmEntCat      ∷ !EntityCatalogue
    , lcmChunkGen    ∷ !ChunkGenerator
      -- | Note that this is an STM map. Accessing it requires an STM
      -- transaction.
    , lcmCells       ∷ !(Map ChunkPos ChunkCell)
      -- | A semaphore for limiting the number of concurrent chunk
      -- generation and loading.
    , lcmSemInFlight ∷ !TSem
    }

instance Show LocalChunkManager where
  showsPrec d lcm
    = showParen (d > appPrec) $
      showString "LocalChunkManager " ∘
      showString "{ lcmTileReg = "  ∘ showsPrec (appPrec + 1) (lcmTileReg  lcm) ∘
      showString ", lcmTilePal = "  ∘ showsPrec (appPrec + 1) (lcmTilePal  lcm) ∘
      showString ", lcmBiomeReg = " ∘ showsPrec (appPrec + 1) (lcmBiomeReg lcm) ∘
      showString ", lcmBiomePal = " ∘ showsPrec (appPrec + 1) (lcmBiomePal lcm) ∘
      showString ", lcmEntCat = "   ∘ showsPrec (appPrec + 1) (lcmEntCat   lcm) ∘
      showString ", .. }"
    where
      appPrec = 10

-- | Construct a new chunk manager out of registries, and a properly
-- populated tile palette and catalogues. Care must be taken because
-- if the palette is somehow invalid it will later result in
-- exceptions (or even chunk corruptions!) but not immediately.
new ∷ TileRegistry
    → TilePalette
    → BiomeRegistry
    → BiomePalette
    → EntityCatalogue
    → ChunkGenerator
    → STM LocalChunkManager
new tReg tPal bReg bPal eCat cGen
  = do cells ← SM.new
       sem   ← newTSem =<< fromIntegral <$> unsafeIOToSTM getNumCapabilities
       return $ LocalChunkManager
         { lcmTileReg     = tReg
         , lcmTilePal     = tPal
         , lcmBiomeReg    = bReg
         , lcmBiomePal    = bPal
         , lcmEntCat      = eCat
         , lcmChunkGen    = cGen
         , lcmCells       = cells
         , lcmSemInFlight = sem
         }

-- | Get the chunk at a certain position if it's available. This
-- doesn't block nor has any side effects.
lookup ∷ ChunkPos → LocalChunkManager → STM (Maybe ChunkStatus)
lookup pos lcm
  = do mCell ← SM.lookup pos (lcmCells lcm)
       traverse eval mCell
  where
    eval ∷ ChunkCell → STM ChunkStatus
    eval = either throwSTM return

-- | Get the chunk at a certain position. If it's
-- not already loaded, a chunk will be loaded or generated on a
-- separate thread and the transaction will be retried.
get ∷ ChunkPos → LocalChunkManager → STM Chunk
get pos lcm
  -- Loading chunks involves I/O so we have to do it outside of a
  -- transaction.
  = do mCell ← SM.lookup pos (lcmCells lcm)
       case mCell of
         Just (Right  Loading)   → retry
         Just (Right (Loaded c)) → return c
         Just (Left e)           → throwSTM e
         Nothing →
           do void $ unsafeIOToSTM $ forkIO $
                -- Now we are in a separate thread. First put an empty
                -- cell in the map so no more transactions will
                -- fork. But here is a catch: this forkIO can race so
                -- at the time when we're here the cell can already be
                -- populated.
                do raced ← atomically $
                           do raced ← SM.focus markAsLoading pos (lcmCells lcm)
                              if raced
                                then return True
                                -- Also limit the number of
                                -- simultaneously running threads.
                                else waitTSem (lcmSemInFlight lcm) *> return False
                   if raced
                     then return ()
                     -- The cell has just been marked as loading,
                     -- which guarantees there are no other threads
                     -- that are loading or generating this chunk.
                     else do cell ← (Right ∘ Loaded <$> loadOrGenerate)
                                    `catch`
                                    (return ∘ Left)
                             atomically $
                               do SM.insert cell pos (lcmCells lcm)
                                  signalTSem (lcmSemInFlight lcm)
              -- We just spawned a thread to populate the
              -- cell. Hopefully sleep until someone modifies the map.
              retry
  where
    loadOrGenerate ∷ (MonadThrow μ, MonadIO μ) ⇒ μ Chunk
    loadOrGenerate = generate pos lcm -- FIXME: load

    markAsLoading ∷ F.Focus ChunkCell STM Bool
    markAsLoading
      = do raced ← F.member
           if raced
             then return True
             else F.insert (Right Loading) *> return False

-- | Put or overwrite a chunk at a certain position.
put ∷ ChunkPos → Chunk → LocalChunkManager → STM ()
put pos chunk lcm
  = SM.insert (Right $ Loaded chunk) pos (lcmCells lcm)

-- | Apply a modification to the chunk at a certain position. If it's
-- not already loaded, a chunk will be loaded or generated on a
-- separate thread and the transaction will be retried.
modify ∷ (Chunk → Chunk) → ChunkPos → LocalChunkManager → STM ()
modify f pos lcm
  = do chunk ← get pos lcm
       SM.insert (Right $ Loaded $ f chunk) pos (lcmCells lcm)

-- | Generate a chunk. Since chunk generation is a time consuming
-- task, callers are highly advised to do it in a separate thread.
generate ∷ (MonadThrow μ, MonadIO μ)
         ⇒ ChunkPos
         → LocalChunkManager
         → μ Chunk
generate cPos (LocalChunkManager { .. })
  = generateChunk lcmTileReg lcmTilePal lcmBiomeReg lcmBiomePal lcmEntCat lcmChunkGen cPos
