{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Player.Manager.Local
  ( LocalPlayerManager
  , new
  , lookup
  , get
  , put
  ) where

import Control.Monad.STM (STM, throwSTM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Game.AcidRain.World (UnknownPlayerIDException(..))
import Game.AcidRain.World.Player (Player(..), PlayerID)
import Prelude hiding (lookup)


-- FIXME: Switch to http://hackage.haskell.org/package/stm-containers
data LocalPlayerManager
  = LocalPlayerManager
    { lpmPlayers ∷ !(TVar (HashMap PlayerID Player))
    }

instance Show LocalPlayerManager where
  showsPrec d _
    = showParen (d > appPrec) $
      showString "LocalPlayerManager { .. }"
    where
      appPrec = 10

-- | Create an empty player manager.
new ∷ STM LocalPlayerManager
new = do pls ← newTVar HM.empty
         return LocalPlayerManager { lpmPlayers = pls }

-- | Lookup a player in the world having a given ID. The Nil UUID is
-- not a special-case in this function.
lookup ∷ PlayerID → LocalPlayerManager → STM (Maybe Player)
lookup pid lpm
  = HM.lookup pid <$> (readTVar $ lpmPlayers lpm)

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
  = modifyTVar' (lpmPlayers lpm) put'
  where
    put' = HM.insert (plID pl) pl
