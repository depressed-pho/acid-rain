{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Window.HUD.DebugInfo
  ( DebugInfo
  , debugInfo
  ) where

import Brick.Types (Widget)
import Brick.Widgets.Core (Named(..), emptyWidget)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Unique (Unique, newUnique)
import Game.AcidRain.TUI.Window (Window(..), WindowType(..))
import Game.AcidRain.World (World(..), SomeWorld)
import Game.AcidRain.World.Player (PlayerID)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((∘))


data DebugInfo
  = DebugInfo
    { _diName   ∷ !Unique
    , _diWorld  ∷ !SomeWorld
    , _diPlayer ∷ !PlayerID
    , _diWidget ∷ !(Maybe (Widget Unique))
    }

makeLenses ''DebugInfo

instance Named DebugInfo Unique where
  getName = (^.diName)

instance Window DebugInfo where
  windowID   _ = "acid-rain:debug-info"
  windowType _ = HUD
  renderWindow = (:[]) ∘ fromMaybe emptyWidget ∘ (^.diWidget)

debugInfo ∷ (World w, MonadIO μ) ⇒ w → PlayerID → μ DebugInfo
debugInfo w pid
  = do n ← liftIO $ newUnique
       return DebugInfo
         { _diName   = n
         , _diWorld  = upcastWorld w
         , _diPlayer = pid
         , _diWidget = Nothing
         }
