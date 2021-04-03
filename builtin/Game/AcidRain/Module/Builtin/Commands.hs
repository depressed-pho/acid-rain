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
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module.Loader (LoaderContext, registerCommand)
import Game.AcidRain.TUI.Keystroke (keyQ)
import Game.AcidRain.World.Command
  ( Command(..), CommandType(..), BadArgumentsException(..)
  , throwSomeExc, getPlayer, tryMoveEntity )
import Game.AcidRain.World.Player (plPos)
import Game.AcidRain.World.Position (wpY)
import Lens.Micro ((^.), (&), (<&>), (-~))


data WalkNorth
instance Command (Proxy WalkNorth) where
  commandID   _ = "acid-rain:walk-north"
  commandType _ = Interactive $ Just [keyQ|k|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpY -~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

loadCommands ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadCommands
  = traverse_ registerCommand
    [ upcastCommand (Proxy ∷ Proxy WalkNorth)
    ]
