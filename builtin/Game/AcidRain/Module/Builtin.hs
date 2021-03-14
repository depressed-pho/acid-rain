{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin
  ( BuiltinModule
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadState)
import Game.AcidRain.Module (Module(..), ModuleID)
import Game.AcidRain.Module.Builtin.Loader.Tile (loadTiles)
import Game.AcidRain.Module.Loader (LoaderContext)


-- | This is a built-in module of Acid Rain whose existence is,
-- strictly speaking, not mandatory but realistically is.
data BuiltinModule

instance Module BuiltinModule where
  modID ∷ BuiltinModule → ModuleID
  modID _ = "acid-rain"

  load ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ α → μ ()
  load _ = loadTiles
