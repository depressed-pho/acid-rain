{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Local
  ( LocalWorld
  , newWorld
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (STM, atomically, throwSTM)
import qualified Data.UUID as U
import Game.AcidRain.World (World(..), WorldMode(..), UnknownPlayerIDException(..))
import Game.AcidRain.World.Chunk (Chunk)
import Game.AcidRain.World.Chunk.Manager.Local (LocalChunkManager)
import qualified Game.AcidRain.World.Chunk.Manager.Local as LCM
import Game.AcidRain.World.Chunk.Position (ChunkPos)
import Game.AcidRain.World.Player.Manager.Local (LocalPlayerManager)
import Game.AcidRain.World.Player (Player(..), Permission(..), PlayerID)
import Game.AcidRain.World.Position (WorldPos(..))
import qualified Game.AcidRain.World.Player.Manager.Local as LPM


-- | Local world is a server-side world which is owned by a
-- server. The server accesses the world data directly. It is used
-- both in single player mode and multi player mode.
data LocalWorld
  = LocalWorld
    { lwMode    ∷ !WorldMode
    , lwChunks  ∷ !LocalChunkManager
    , lwPlayers ∷ !LocalPlayerManager
    }

instance World LocalWorld where
  lookupChunk ∷ MonadIO μ ⇒ ChunkPos → LocalWorld → μ (Maybe Chunk)
  lookupChunk pos lw
    = liftIO $ atomically $ LCM.lookup pos $ lwChunks lw

  -- FIXME: Remove this later.
  ensureChunkExists pos lw
    = liftIO $ atomically $ LCM.ensureChunkExists pos $ lwChunks lw

  getPlayer ∷ (MonadIO μ, MonadThrow μ) ⇒ PlayerID → LocalWorld → μ Player
  getPlayer pid lw
    = liftIO $ atomically $ get'
    where
      get' | U.null pid = case lwMode lw of
                            SinglePlayer →
                              do pl' ← LPM.lookup pid $ lwPlayers lw
                                 case pl' of
                                   -- The Nil player already exists.
                                   Just pl → return pl
                                   -- Create the Nil player.
                                   Nothing → newPlayer U.nil Administrator lw
                            MultiPlayer →
                              -- Nil player can't exist in multi
                              -- player mode.
                              throwSTM $ UnknownPlayerIDException pid

           | otherwise  = case lwMode lw of
                            SinglePlayer →
                              -- Only the Nil player can exist in
                              -- single player mode.
                              throwSTM $ UnknownPlayerIDException pid
                            MultiPlayer →
                              -- Throw if the player doesn't exist.
                              LPM.get pid $ lwPlayers lw

-- | Create a new world out of thin air.
newWorld ∷ WorldMode → a
newWorld = error "FIXME"

-- | Get the coordinate of the initial spawn.
initialSpawn ∷ LocalWorld → STM WorldPos
initialSpawn _lw
  -- FIXME: The initial spawn point can only be determined after
  -- generating the spawn chunk.
  = return $ WorldPos 0 0 0

-- | Spawn a new player in the world.
newPlayer ∷ PlayerID → Permission → LocalWorld → STM Player
newPlayer pid perm lw
  = do spawn ← initialSpawn lw
       let pl = Player
                { plID   = pid
                , plPerm = perm
                , plPos  = spawn
                }
       LPM.put pl $ lwPlayers lw
       return pl
