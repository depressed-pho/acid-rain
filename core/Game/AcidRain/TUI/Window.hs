{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Window
  ( Window(..)
  , WindowType(..)
  , WindowID
  ) where

import Brick.Types (Widget, EventM)
import Brick.Widgets.Core (Named(..))
import Data.Poly.Strict (Poly(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Unique (Unique)
import qualified Game.AcidRain.World.Event as WE
import qualified Graphics.Vty as V
import Prelude.Unicode ((∘))


type WindowID = Text

-- | 'Window' is a Brick widget that is shown as a part of
-- 'Game.AcidRain.TUI.Client'. A HUD, or an item selection window, are
-- examples of 'Window'.
--
-- Note that the Brick widget name is specialized to 'Unique'. We
-- don't like this, but this is unavoidable because making it
-- polymorphic propagates a polymorphic name @n@ all the way to
-- 'Game.AcidRain.World.World' which logically doesn't make sense.
class Named w Unique ⇒ Window w where
  -- | Erase the type of the window.
  upcastWindow ∷ w → Poly Window
  upcastWindow = Poly
  -- | Get the window ID like "acid-rain:debug-info"
  windowID ∷ w → WindowID
  -- | Get the window type.
  windowType ∷ w → WindowType
  -- | Render the window.
  renderWindow ∷ w → Seq (Widget Unique)
  -- | Invoked when the window is inserted to the client before the
  -- window is first rendered.
  windowStartEvent ∷ w → EventM Unique w
  -- | Handle a world event. Unlike Vty events, world events are
  -- always propagated through all the windows and the client itself.
  handleWorldEvent ∷ w → Poly WE.Event → EventM Unique w
  -- | Handle a Vty event. They are propagated through modal windows
  -- and finally to the client itself. At any time a modal window can
  -- stop the event propagation by returning @(w, 'False')@. The
  -- default implementation does nothing and just propagate the event
  -- to the next window.
  handleVtyEvent ∷ w → V.Event → EventM Unique (w, Bool)
  handleVtyEvent w _ = return (w, True)

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

instance Named (Poly Window) Unique where
  getName (Poly w) = getName w

instance Window (Poly Window) where
  upcastWindow = id
  windowID (Poly w) = windowID w
  windowType (Poly w) = windowType w
  renderWindow (Poly w) = renderWindow w
  windowStartEvent (Poly w) = Poly <$> windowStartEvent w
  handleWorldEvent (Poly w) = (Poly <$>) ∘ handleWorldEvent w
  handleVtyEvent (Poly w) ev
    = do (w', pr) ← handleVtyEvent w ev
         return (Poly w', pr)
