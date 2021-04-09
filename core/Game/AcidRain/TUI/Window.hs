{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Window
  ( Window(..)
  , WindowType(..)
  , WindowID
  , SomeWindow(..)
  ) where

import Brick.Types (Widget)
import Brick.Widgets.Core (Named(..))
import Data.Text (Text)
import Data.Unique (Unique)


type WindowID = Text

-- | 'Window' is a Brick widget that is shown as a part of
-- 'Game.AcidRain.TUI.Client'. A HUD, or an item selection window, are
-- examples of 'Window'.
class Named w Unique ⇒ Window w where
  -- | Erase the type of the window.
  upcastWindow ∷ w → SomeWindow
  upcastWindow = SomeWindow
  -- | Get the window ID like "acid-rain:debug-info"
  windowID ∷ w → WindowID
  -- | Get the window type.
  windowType ∷ w → WindowType
  -- | Render the window.
  renderWindow ∷ w → [Widget Unique]

-- | Window type. This affects the order of windows stacked on the
-- screen, at least the default of it.
data WindowType
  = -- | HUD windows are always behind modal ones, and they never get
    -- focus. They receive world events but will never receive Vty
    -- events.
    HUD
    -- | Modal windows can receive focus. When they are focused, Vty
    -- events are first sent to them before
    -- 'Game.AcidRain.TUI.Widgets.WorldView'.
  | Modal
  deriving (Eq, Show)

-- | Type-erased 'Window'.
data SomeWindow = ∀w. Window w ⇒ SomeWindow !w

instance Named SomeWindow Unique where
  getName (SomeWindow w) = getName w

instance Window SomeWindow where
  upcastWindow = id
  windowID (SomeWindow w) = windowID w
  windowType (SomeWindow w) = windowType w
  renderWindow (SomeWindow w) = renderWindow w
