{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Player.Manager.Local
  ( LocalPlayerManager

    -- * Construction
  , new

    -- * Query
  , lookup
  , get
  , getSubscribers

    -- * Update
  , put
  , modify
  , subscribeToChunks
  ) where

import Control.Monad.STM (STM, throwSTM)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified DeferredFolds.UnfoldlM as DFM
import qualified Focus as F
import Game.AcidRain.World (UnknownPlayerIDException(..), PlayerOfflineException(..))
import Game.AcidRain.World.Chunk.Position (ChunkPos, cpX, cpY)
import Game.AcidRain.World.Player (Player(..), PlayerID, plID, plIsOnline)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Prelude hiding (lookup)
import Prelude.Unicode ((∘), (≥), (≤), (∧))
import StmContainers.Map (Map)
import qualified StmContainers.Map as SM


data LocalPlayerManager
  = LocalPlayerManager
    { -- | All the known players in this world, including those who
      -- are currently offline. Note that this is an STM
      -- map. Accessing it requires an STM transaction.
      _lpmPlayers   ∷ !(Map PlayerID Player)
      -- | Chunk subscription @(topLeft, bottomRight)@ for each online
      -- player.
    , _lpmChunkSubs ∷ !(Map PlayerID (ChunkPos, ChunkPos))
    }

makeLenses ''LocalPlayerManager

instance Show LocalPlayerManager where
  showsPrec d _
    = showParen (d > appPrec) $
      showString "LocalPlayerManager { .. }"
    where
      appPrec = 10

-- | Create an empty player manager.
new ∷ STM LocalPlayerManager
new = do pls  ← SM.new
         subs ← SM.new
         return $ LocalPlayerManager pls subs

-- | Lookup a player in the world having a given ID. The Nil UUID is
-- not a special-case in this function.
lookup ∷ PlayerID → LocalPlayerManager → STM (Maybe Player)
lookup pid
  = SM.lookup pid ∘ (^.lpmPlayers)

-- | Get a player in the world having a given ID, or throw if not
-- found. The Nil UUID is not a special case in this function.
get ∷ PlayerID → LocalPlayerManager → STM Player
get pid lpm
  = do mPl ← lookup pid lpm
       case mPl of
         Just pl → return pl
         Nothing → throwSTM $ UnknownPlayerIDException pid

-- | Get a HashSet of all the online players who have subscribed to
-- a given chunk.
getSubscribers ∷ ChunkPos → LocalPlayerManager → STM (HashSet PlayerID)
getSubscribers cPos lpm
  = DFM.foldlM' go HS.empty $ SM.unfoldlM (lpm^.lpmChunkSubs)
  where
    go ∷ HashSet PlayerID → (PlayerID, (ChunkPos, ChunkPos)) → STM (HashSet PlayerID)
    go set (pid, (topLeft, bottomRight))
      | cPos^.cpX ≥ topLeft    ^.cpX ∧
        cPos^.cpX ≤ bottomRight^.cpX ∧
        cPos^.cpY ≥ topLeft    ^.cpY ∧
        cPos^.cpY ≤ bottomRight^.cpY = return $ HS.insert pid set
      | otherwise                    = return set

-- | Insert or overwrite a player record.
put ∷ Player → LocalPlayerManager → STM ()
put pl lpm
  = SM.insert pl (pl^.plID) (lpm^.lpmPlayers)

-- | Modify a player record, or throw if no such player exists.
modify ∷ (Player → Player) → PlayerID → LocalPlayerManager → STM ()
modify f pid lpm
  = do modified ← SM.focus focMod pid (lpm^.lpmPlayers)
       case modified of
         Just pl →
           if not (pl^.plIsOnline)
           then SM.delete pid (lpm^.lpmChunkSubs)
           else return ()
         Nothing →
           throwSTM $ UnknownPlayerIDException pid
  where
    focMod = do F.adjust f
                F.lookup

-- | Update the chunk subscription for a given online player, or throw
-- if the player is offline or doesn't exist.
subscribeToChunks ∷ PlayerID
                  → (ChunkPos, ChunkPos) -- ^ @(topLeft, bottomRight)@
                  → LocalPlayerManager
                  → STM ()
subscribeToChunks pid chunks lpm
  = do pl ← get pid lpm
       if pl^.plIsOnline
         then SM.insert chunks pid (lpm^.lpmChunkSubs)
         else throwSTM $ PlayerOfflineException pid
