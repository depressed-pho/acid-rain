{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Commands
  ( loadCommands
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module.Loader (LoaderContext, registerCommand)
import Game.AcidRain.TUI.Keystroke (keyQ)
import Game.AcidRain.World.Command (Command(..), CommandType(..))


data WalkNorth
instance Command (Proxy WalkNorth) where
  commandID   _ = "acid-rain:walk-north"
  commandType _ = Interactive $ Just [keyQ|k|]

loadCommands ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadCommands
  = traverse_ registerCommand
    [ upcastCommand (Proxy ∷ Proxy WalkNorth)
    ]
