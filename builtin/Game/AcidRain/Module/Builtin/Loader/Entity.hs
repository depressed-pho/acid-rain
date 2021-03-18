{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Loader.Entity
  ( loadEntities
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module.Loader (LoaderContext, registerEntityType)
import Game.AcidRain.TUI
  ( HasAppearance(..), begin, end, (⊳), unicode, ascii, bold )
import Game.AcidRain.World.Entity (EntityType(..), Entity(..))


data PlayerT
instance EntityType (Proxy PlayerT) where
  type EntityOf (Proxy PlayerT) = Player
  entityTypeID _ = "acid-rain:player"
  spawnEntity _ = error "FIXME"

data Player
instance Entity Player where
  type EntityTypeOf Player = Proxy PlayerT
  entityType _ = Proxy

instance HasAppearance Player where
  appearance _
    = begin ⊳ unicode "@" ⊳ ascii '@' ⊳ bold ⊳ end

loadEntities ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ μ ()
loadEntities
  = traverse_ registerEntityType
    [ upcastEntityType (Proxy ∷ Proxy PlayerT)
    ]
