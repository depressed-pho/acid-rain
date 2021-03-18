{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin
  ( BuiltinModule
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadState)
import Data.Proxy (Proxy)
import Game.AcidRain.Module (Module(..), ModuleID)
import Game.AcidRain.Module.Builtin.Loader.Tile (loadTiles)
import Game.AcidRain.Module.Loader (LoaderContext)


-- | This is a built-in module of Acid Rain whose existence is
-- mandatory because it's used by the base game logic.
data BuiltinModule

instance Module (Proxy BuiltinModule) where
  modID ∷ Proxy BuiltinModule → ModuleID
  modID _ = "acid-rain"

  load ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ Proxy BuiltinModule → μ ()
  load _ = loadTiles
