{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Window
  ( Window(..)
  , SomeWindow(..)
  ) where

import Brick.Types (Widget)
import Brick.Widgets.Core (Named(..))


-- | 'Window' is a Brick widget that is shown as a part of
-- 'Game.AcidRain.TUI.Client'. A HUD, or an item selection window, are
-- examples of 'Window'.
class Named w n ⇒ Window w n where
  -- | Erase the type of the window.
  upcastWindow ∷ w → SomeWindow n
  upcastWindow = SomeWindow
  -- | Render the window.
  renderWindow ∷ w → [Widget n]

data SomeWindow n = ∀w. Window w n ⇒ SomeWindow !w

instance Named (SomeWindow n) n where
  getName (SomeWindow w) = getName w

instance Window (SomeWindow n) n where
  upcastWindow = id
  renderWindow (SomeWindow w) = renderWindow w
