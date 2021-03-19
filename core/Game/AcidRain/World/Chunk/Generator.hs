{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk.Generator
  ( ChunkGenerator(..), terraform, decorate
  , ChunkGenM

    -- * Running chunk generators
  , generateChunk
  ) where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Trans.State.Strict (StateT, execStateT)
import Control.Monad.Trans.Class (lift)
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
import Prelude.Unicode ((∘))


-- | The chunk generator monad is essentially a state monad to
-- manipulate chunks efficiently.
newtype ChunkGenM α
  = ChunkGenM { runChunkGenM ∷ StateT (ChunkGenState (PrimState IO)) IO α }
  deriving (Functor, Applicative, Monad, MonadFail, MonadThrow, MonadCatch)

-- For some reason GeneralisedNewtypeDeriving doesn't work for
-- PrimMonad. It just emits a cryptic compilation error.
instance PrimMonad ChunkGenM where
  type PrimState ChunkGenM = PrimState (StateT (ChunkGenState (PrimState IO)) IO)
  primitive = ChunkGenM ∘ lift ∘ primitive

data ChunkGenState σ
  = ChunkGenState
    { _cgPos   ∷ ChunkPos
    , _cgChunk ∷ MutableChunk σ
    }

makeLenses ''ChunkGenState

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
    { _terraform ∷ ChunkGenM ()
    , _decorate  ∷ ChunkGenM ()
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
       cgs' ← liftIO $ flip execStateT cgs $
              do runChunkGenM $ cGen^.terraform
                 runChunkGenM $ cGen^.decorate
       liftIO $ freezeChunk $ cgs'^.cgChunk
