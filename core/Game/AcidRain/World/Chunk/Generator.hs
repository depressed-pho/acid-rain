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

    -- * Running chunk generators
  , generateChunk
  ) where

import Control.Eff (Eff, Lift, runLift)
import Control.Eff.Instances.Catch ()
import Control.Eff.State.Strict (State, execState)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Default (Default(..))
import qualified Game.AcidRain.World.Chunk as C
import Game.AcidRain.World.Chunk.Types (freezeChunk, thawChunk)
import Game.AcidRain.World.Chunk.Palette (TilePalette)
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Chunk.Types (Chunk, MutableChunk)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue)
import Game.AcidRain.World.Tile (defaultState)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

data ChunkGenState σ
  = ChunkGenState
    { _cgPos   ∷ ChunkPos
    , _cgChunk ∷ MutableChunk σ
    }

makeLenses ''ChunkGenState

-- | The 'ChunkGen' monad is essentially a state monad for
-- manipulating chunks efficiently.
newtype ChunkGenM α
  -- The fact it's actually a lifted IO monad is totally an
  -- implementation detail. We just need some PrimMonad to mutate
  -- vectors but we don't want to expose that to outside.
  = ChunkGenM { runChunkGenM ∷ Eff '[State (ChunkGenState (PrimState IO)), Lift IO] α }
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

-- | Generate a chunk by running a chunk generator. Only useful for
-- server implementations.
generateChunk ∷ (MonadThrow μ, MonadIO μ)
              ⇒ TileRegistry
              → TilePalette
              → EntityCatalogue
              → ChunkGenerator
              → ChunkPos
              → μ Chunk
generateChunk tReg tPal eCat cGen cPos
  = do air  ← TR.get "acid-rain:dirt" tReg
       c    ← C.new tReg tPal eCat (defaultState air)
       mc   ← liftIO $ thawChunk c
       let cgs = ChunkGenState
                 { _cgPos   = cPos
                 , _cgChunk = mc
                 }
       cgs' ← liftIO $ runLift $ execState cgs $ runChunkGenM $ runLift $
              do cGen^.terraform
                 cGen^.decorate
       liftIO $ freezeChunk $ cgs'^.cgChunk
