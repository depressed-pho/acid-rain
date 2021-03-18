{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Entity
  ( PlayerT, Player(..)
  ) where

import Data.Proxy (Proxy(..))
import Game.AcidRain.TUI
  ( HasAppearance(..), begin, end, (⊳), unicode, ascii, bold )
import Game.AcidRain.World.Entity (EntityType(..), Entity(..))
import Game.AcidRain.World.Player (PlayerID)


data PlayerT
instance EntityType (Proxy PlayerT) where
  type EntityOf (Proxy PlayerT) = Player
  entityTypeID _ = "acid-rain:player"

data Player = Player !PlayerID deriving Show
instance Entity Player where
  type EntityTypeOf Player = Proxy PlayerT
  entityType _ = Proxy

instance HasAppearance Player where
  appearance _
    = begin ⊳ unicode "@" ⊳ ascii '@' ⊳ bold ⊳ end
