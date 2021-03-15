{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.Widgets.WorldView
  ( WorldView
  , worldView
  , renderWorldView
  ) where

import Brick.Types (Location(..), Widget)
import Brick.Widgets.Core (Named(..), emptyWidget)
import Game.AcidRain.World (World(..), SomeWorld)
import Game.AcidRain.World.Player (PlayerID)


data WorldView n
  = WorldView
    { wvName   ∷ !n
    , wvWorld  ∷ !SomeWorld
      -- | The player to trac.
    , wvPlayer ∷ !PlayerID
      -- | The offset from the center of the widget where the player
      -- should be located.
    , wvPlayerOffset ∷ !Location
    }

instance Named (WorldView n) n where
  getName = wvName

worldView ∷ World w ⇒ n → w → PlayerID → WorldView n
worldView n w pid
  = WorldView
    { wvName         = n
    , wvWorld        = upcastWorld w
    , wvPlayer       = pid
    , wvPlayerOffset = Location (0, 0)
    }

renderWorldView ∷ WorldView n → Widget n
renderWorldView _wv
  = emptyWidget
