{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk.Generator
  ( ChunkGenerator(..), terraform, decorate
  , ChunkGenM

    -- * Querying state
  , chunkPos
  , tileRegistry

    -- * Modifying state
  , putTileState
  , putClimate
  , putRiver
  , putBiome

    -- * Running chunk generators
  , generateChunk
  ) where

import Control.Eff (Eff, Lift, Lifted, lift, runLift)
import Control.Eff.Instances.Catch ()
import Control.Eff.State.Strict (State, execState, get)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Default (Default(..))
import Game.AcidRain.World.Biome (Biome)
import Game.AcidRain.World.Biome.Palette (BiomePalette)
import Game.AcidRain.World.Biome.Registry (BiomeRegistry)
import qualified Game.AcidRain.World.Biome.Registry as BR
import qualified Game.AcidRain.World.Chunk as C
import Game.AcidRain.World.Chunk.Types
  ( TileOffset, mcTileReg, freezeChunk, thawChunk
  , writeTileState, writeClimate, writeRiver, writeBiome )
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Chunk.Types (Chunk, MutableChunk)
import Game.AcidRain.World.Climate (Climate)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue)
import Game.AcidRain.World.Tile (TileState, defaultState)
import Game.AcidRain.World.Tile.Palette (TilePalette)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

data ChunkGenState
  = ChunkGenState
    { _cgPos   ∷ ChunkPos
    , _cgChunk ∷ MutableChunk (PrimState IO)
    }

makeLenses ''ChunkGenState

-- | The 'ChunkGen' monad is essentially a state monad for
-- manipulating chunks efficiently.
newtype ChunkGenM α
  -- The fact it's actually a lifted IO monad is totally an
  -- implementation detail. We just need some PrimMonad to mutate
  -- vectors but we don't want to expose that to outside.
  = ChunkGenM { runChunkGenM ∷ Eff '[State ChunkGenState, Lift IO] α }
  deriving (Functor, Applicative, Monad, MonadThrow)

-- | Chunks start with an empty state where every tile position is
-- filled with @acid-rain:air@ and no entities exist. The task of
-- 'ChunkGenerator' is to generate terrain and resources in chunks.
--
-- Chunk generation is divided into the following steps:
--
-- ['terraform'] Generate the terrain. After this step completes,
-- chunks have flats, mountains, rivers, lakes, and things like that.
--
-- ['decorate'] Generate some decorations. Ore veins are generated,
-- trees are planted, and grass grow on this step.
--
-- 'ChunkGenerator' is a property of a
-- 'Game.AcidRain.World.World'. When modules are to be loaded, they
-- can mutate the 'ChunkGenerator' to inject some code to run. They
-- can even replace the entire generator if they want to, which is
-- usually undesirable though.
data ChunkGenerator
  = ChunkGenerator
    { _terraform ∷ Eff '[Lift ChunkGenM] ()
    , _decorate  ∷ Eff '[Lift ChunkGenM] ()
    }

makeLenses ''ChunkGenerator

instance Default ChunkGenerator where
  def = ChunkGenerator
        { _terraform = return ()
        , _decorate  = return ()
        }

-- | Query the position of the chunk to generate.
chunkPos ∷ Lifted ChunkGenM r ⇒ Eff r ChunkPos
chunkPos = lift $ ChunkGenM $ (^.cgPos) <$> get

-- | Query the tile registry for the world of which the chunk is being
-- generated.
tileRegistry ∷ Lifted ChunkGenM r ⇒ Eff r TileRegistry
tileRegistry = lift $ ChunkGenM $ (^.cgChunk.mcTileReg) <$> get

-- | Put a tile state at a given tile offset.
putTileState ∷ Lifted ChunkGenM r ⇒ TileOffset → TileState τ → Eff r ()
putTileState off ts
  = lift $ ChunkGenM $
    do mc ← (^.cgChunk) <$> get
       lift $ writeTileState off ts mc

-- | Put a climate value at a given @(x, y)@ tile offset.
putClimate ∷ Lifted ChunkGenM r ⇒ TileOffset → Climate → Eff r ()
putClimate off cli
  = lift $ ChunkGenM $
    do mc ← (^.cgChunk) <$> get
       lift $ writeClimate off cli mc

-- | Put a river strength value at a given @(x, y)@ tile offset.
putRiver ∷ Lifted ChunkGenM r ⇒ TileOffset → Float → Eff r ()
putRiver off cli
  = lift $ ChunkGenM $
    do mc ← (^.cgChunk) <$> get
       lift $ writeRiver off cli mc

-- | Put a biome type at a given @(x, y)@ tile offset.
putBiome ∷ (Biome β, Lifted ChunkGenM r) ⇒ TileOffset → β → Eff r ()
putBiome off b
  = lift $ ChunkGenM $
    do mc ← (^.cgChunk) <$> get
       lift $ writeBiome off b mc

-- | Generate a chunk by running a chunk generator. Only useful for
-- server implementations.
generateChunk ∷ (MonadThrow μ, MonadIO μ)
              ⇒ TileRegistry
              → TilePalette
              → BiomeRegistry
              → BiomePalette
              → EntityCatalogue
              → ChunkGenerator
              → ChunkPos
              → μ Chunk
generateChunk tReg tPal bReg bPal eCat cGen cPos
  = do air    ← TR.get "acid-rain:air" tReg
       plains ← BR.get "acid-rain:plains" bReg
       c      ← C.new tReg tPal bReg bPal eCat (defaultState air) plains
       mc     ← liftIO $ thawChunk c
       let cgs = ChunkGenState
                 { _cgPos   = cPos
                 , _cgChunk = mc
                 }
       cgs' ← liftIO $ runLift $ execState cgs $ runChunkGenM $ runLift $
              do cGen^.terraform
                 cGen^.decorate
       liftIO $ freezeChunk $ cgs'^.cgChunk
