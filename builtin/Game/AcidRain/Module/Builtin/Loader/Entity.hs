{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Loader.Entity
  ( loadEntities
  ) where

import Control.Eff (Eff, type (<::))
import Control.Eff.Exception (Exc)
import Control.Eff.State.Strict (State)
import Control.Exception (SomeException)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import qualified Game.AcidRain.Module.Builtin.Entity as E
import Game.AcidRain.World.Entity (EntityType(..))
import Game.AcidRain.Module.Loader (LoaderContext, registerEntityType)


loadEntities ∷ [State LoaderContext, Exc SomeException] <:: r ⇒ Eff r ()
loadEntities
  = traverse_ registerEntityType
    [ upcastEntityType (Proxy ∷ Proxy E.PlayerT)
    ]
