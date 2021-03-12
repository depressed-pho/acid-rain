{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Local
  ( LocalWorld
  , new
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Game.AcidRain.World (World(..))
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Manager.Local (LocalChunkManager)
import qualified Game.AcidRain.World.Chunk.Manager.Local as LCM
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Player.Manager.Local (LocalPlayerManager)
import Game.AcidRain.World.Player (Player, PlayerID)
import qualified Game.AcidRain.World.Player.Manager.Local as LPM
import Prelude.Unicode ((∘))


-- | Local world is a server-side world which is owned by a
-- server. The server accesses the world data directly.
data LocalWorld
  = LocalWorld
    { lwChunks  ∷ !LocalChunkManager
    , lwPlayers ∷ !LocalPlayerManager
    }

instance World LocalWorld where
  lookupChunk ∷ MonadIO μ ⇒ ChunkPos → LocalWorld → μ (Maybe Chunk)
  lookupChunk pos lw
    = liftIO $ atomically $ LCM.lookup pos $ lwChunks lw

  -- FIXME: Remove this later.
  ensureChunkExists pos lw
    = liftIO $ atomically $ LCM.ensureChunkExists pos $ lwChunks lw

  getRootPlayer ∷ MonadIO μ ⇒ LocalWorld → μ Player
  getRootPlayer
    = liftIO ∘ atomically ∘ LPM.getRoot ∘ lwPlayers

  getPlayer ∷ (MonadIO μ, MonadThrow μ) ⇒ PlayerID → LocalWorld → μ Player
  getPlayer pid lw
    = liftIO $ atomically $ LPM.get pid $ lwPlayers lw

-- | Create a new world out of thin air.
new = error "FIXME"
