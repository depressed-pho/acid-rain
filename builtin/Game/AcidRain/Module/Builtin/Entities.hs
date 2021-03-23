{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Entities
  ( loadEntities
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import qualified Game.AcidRain.Module.Builtin.Entity as E
import Game.AcidRain.World.Entity (EntityType(..))
import Game.AcidRain.Module.Loader (LoaderContext, registerEntityType)


loadEntities ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadEntities
  = traverse_ registerEntityType
    [ upcastEntityType (Proxy ∷ Proxy E.PlayerT)
    ]
