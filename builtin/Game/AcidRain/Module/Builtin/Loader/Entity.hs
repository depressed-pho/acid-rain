{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Loader.Entity
  ( loadEntities
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import qualified Game.AcidRain.Module.Builtin.Entity as E
import Game.AcidRain.World.Entity (EntityType(..))
import Game.AcidRain.Module.Loader (LoaderContext, registerEntityType)


loadEntities ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ μ ()
loadEntities
  = traverse_ registerEntityType
    [ upcastEntityType (Proxy ∷ Proxy E.PlayerT)
    ]
