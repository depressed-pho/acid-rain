{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Window.HUD.DebugInfo
  ( DebugInfo
  , debugInfo
  ) where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Named(..), emptyWidget, txt, (<+>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid.Unicode ((⊕))
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Unique (Unique, newUnique)
import Game.AcidRain.TUI.Window (Window(..), WindowType(..))
import Game.AcidRain.World (World(..), SomeWorld)
import Game.AcidRain.World.Player (PlayerID, plPos)
import Game.AcidRain.World.Position (WorldPos, wpX, wpY, wpZ)
import Lens.Micro ((&), (^.), (.~), to)
import Lens.Micro.TH (makeLenses)


data DebugInfo
  = DebugInfo
    { _diName   ∷ !Unique
    , _diWorld  ∷ !SomeWorld
    , _diPlayer ∷ !PlayerID
    , _diWidget ∷ !(Widget Unique)
    }

makeLenses ''DebugInfo

instance Named DebugInfo Unique where
  getName = (^.diName)

instance Window DebugInfo where
  windowID   _ = "acid-rain:debug-info"
  windowType _ = HUD
  renderWindow = (^.diWidget.to pure)
  windowStartEvent di   = redrawDebugInfo di
  handleWorldEvent di _ = redrawDebugInfo di

redrawDebugInfo ∷ DebugInfo → EventM Unique DebugInfo
redrawDebugInfo di
  = do pl ← getPlayerFromWorld (di^.diWorld) (di^.diPlayer)
       let w = renderPos (pl^.plPos)
       return $ di & diWidget .~ w

renderPos ∷ WorldPos → Widget n
renderPos wp
  = let b = "("
            ⊕ decimal (wp^.wpX) ⊕ ", "
            ⊕ decimal (wp^.wpY) ⊕ ", "
            ⊕ decimal (wp^.wpZ) ⊕ ")"
    in
      txt $ toStrict $ toLazyText b

debugInfo ∷ (World w, MonadIO μ) ⇒ w → PlayerID → μ DebugInfo
debugInfo w pid
  = do n ← liftIO $ newUnique
       return DebugInfo
         { _diName   = n
         , _diWorld  = upcastWorld w
         , _diPlayer = pid
         , _diWidget = emptyWidget
         }
