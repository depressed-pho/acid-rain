{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Player.Manager.Local
  ( LocalPlayerManager
  , getRoot
  , get
  ) where

import Control.Monad.STM (STM, throwSTM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar)
import Data.UUID ()
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Game.AcidRain.World (UnknownPlayerIDException(..))
import Game.AcidRain.World.Player (Player, PlayerID)


-- | Invariant: there is exactly one root player. Not zero, not two.
data LocalPlayerManager
  = LocalPlayerManager
    { lpmPlayers ∷ !(TVar (HashMap PlayerID Player))
    , lpmRoot    ∷ !(TVar PlayerID)
    }

-- | Get the root player of the world.
getRoot ∷ LocalPlayerManager → STM Player
getRoot lpm
  = do root ← readTVar $ lpmRoot lpm
       get root lpm

-- | Get a player in the world having a given ID.
get ∷ PlayerID → LocalPlayerManager → STM Player
get pid lpm
  = do players ← readTVar $ lpmPlayers lpm
       case HM.lookup pid players of
         Just p  → return p
         Nothing → throwSTM $ UnknownPlayerIDException pid
