{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Player.Manager.Local
  ( LocalPlayerManager
  , new
  , lookup
  , get
  , put
  , modify
  ) where

import Control.Monad.STM (STM, throwSTM)
import qualified Focus as F
import Game.AcidRain.World (UnknownPlayerIDException(..))
import Game.AcidRain.World.Player (Player(..), PlayerID, plID)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Prelude hiding (lookup)
import Prelude.Unicode ((∘))
import StmContainers.Map (Map)
import qualified StmContainers.Map as SM


data LocalPlayerManager
  = LocalPlayerManager
    { -- | Note that this is an STM map. Accessing it requires an STM
      -- transaction.
      _lpmPlayers ∷ !(Map PlayerID Player)
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
new = do pls ← SM.new
         return $ LocalPlayerManager pls

-- | Lookup a player in the world having a given ID. The Nil UUID is
-- not a special-case in this function.
lookup ∷ PlayerID → LocalPlayerManager → STM (Maybe Player)
lookup pid
  = SM.lookup pid ∘ (^.lpmPlayers)

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
  = SM.insert pl (pl^.plID) (lpm^.lpmPlayers)

-- | Modify a player record, or throw if no such player exists.
modify ∷ (Player → Player) → PlayerID → LocalPlayerManager → STM ()
modify f pid lpm
  = do modified ← SM.focus focMod pid (lpm^.lpmPlayers)
       if modified
         then return ()
         else throwSTM $ UnknownPlayerIDException pid
  where
    focMod = do F.adjust f
                F.member
