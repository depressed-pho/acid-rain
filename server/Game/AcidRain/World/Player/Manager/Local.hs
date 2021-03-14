{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Player.Manager.Local
  ( LocalPlayerManager
  , new
  , lookup
  , get
  , put
  ) where

import Control.Monad.STM (STM, throwSTM)
import Game.AcidRain.World (UnknownPlayerIDException(..))
import Game.AcidRain.World.Player (Player(..), PlayerID)
import Prelude hiding (lookup)
import StmContainers.Map (Map)
import qualified StmContainers.Map as SM


data LocalPlayerManager
  = LocalPlayerManager
    { -- | Note that this is an STM map. Accessing it requires an STM
      -- transaction.
      lpmPlayers ∷ !(Map PlayerID Player)
    }

instance Show LocalPlayerManager where
  showsPrec d _
    = showParen (d > appPrec) $
      showString "LocalPlayerManager { .. }"
    where
      appPrec = 10

-- | Create an empty player manager.
new ∷ STM LocalPlayerManager
new = do pls ← SM.new
         return LocalPlayerManager { lpmPlayers = pls }

-- | Lookup a player in the world having a given ID. The Nil UUID is
-- not a special-case in this function.
lookup ∷ PlayerID → LocalPlayerManager → STM (Maybe Player)
lookup pid lpm
  -- Smells like a point-free opportunity, but I don't like unreadable
  -- code.
  = SM.lookup pid $ lpmPlayers lpm

-- | Get a player in the world having a given ID, or throw if not
-- found. The Nil UUID is not a special case in this function.
get ∷ PlayerID → LocalPlayerManager → STM Player
get pid lpm
  = do pl' ← lookup pid lpm
       case pl' of
         Just pl → return pl
         Nothing → throwSTM $ UnknownPlayerIDException pid

-- | Insert or overwrite a player record.
put ∷ Player → LocalPlayerManager → STM ()
put pl lpm
  = SM.insert pl (plID pl) $ lpmPlayers lpm
