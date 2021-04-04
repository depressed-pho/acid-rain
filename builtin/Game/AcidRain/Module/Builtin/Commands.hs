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
import Game.AcidRain.World.Position (wpX, wpY)
import Lens.Micro ((^.), (&), (<&>), (-~), (+~))


data WalkNorth
instance Command (Proxy WalkNorth) where
  commandID   _ = "acid-rain:walk-north"
  commandType _ = Interactive $ Just [keyQ|k|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpY -~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

data WalkWest
instance Command (Proxy WalkWest) where
  commandID   _ = "acid-rain:walk-west"
  commandType _ = Interactive $ Just [keyQ|h|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpX -~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

data WalkEast
instance Command (Proxy WalkEast) where
  commandID   _ = "acid-rain:walk-east"
  commandType _ = Interactive $ Just [keyQ|l|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpX +~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

data WalkSouth
instance Command (Proxy WalkSouth) where
  commandID   _ = "acid-rain:walk-south"
  commandType _ = Interactive $ Just [keyQ|j|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpY +~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

data WalkNorthWest
instance Command (Proxy WalkNorthWest) where
  commandID   _ = "acid-rain:walk-north-west"
  commandType _ = Interactive $ Just [keyQ|y|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpX -~ 1 & wpY -~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

data WalkNorthEast
instance Command (Proxy WalkNorthEast) where
  commandID   _ = "acid-rain:walk-north-east"
  commandType _ = Interactive $ Just [keyQ|u|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpX +~ 1 & wpY -~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

data WalkSouthEast
instance Command (Proxy WalkSouthEast) where
  commandID   _ = "acid-rain:walk-south-west"
  commandType _ = Interactive $ Just [keyQ|n|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpX +~ 1 & wpY +~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

data WalkSouthWest
instance Command (Proxy WalkSouthWest) where
  commandID   _ = "acid-rain:walk-south-east"
  commandType _ = Interactive $ Just [keyQ|b|]
  runOnWorld _ (Just pid) []
    = do pos ← getPlayer pid <&> (^.plPos)
         void $ tryMoveEntity pos (pos & wpX -~ 1 & wpY +~ 1)
  runOnWorld _ _ _
    = throwSomeExc BadArgumentsException

loadCommands ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadCommands
  = traverse_ registerCommand
    [ upcastCommand (Proxy ∷ Proxy WalkNorth)
    , upcastCommand (Proxy ∷ Proxy WalkWest)
    , upcastCommand (Proxy ∷ Proxy WalkEast)
    , upcastCommand (Proxy ∷ Proxy WalkSouth)
    , upcastCommand (Proxy ∷ Proxy WalkNorthWest)
    , upcastCommand (Proxy ∷ Proxy WalkNorthEast)
    , upcastCommand (Proxy ∷ Proxy WalkSouthEast)
    , upcastCommand (Proxy ∷ Proxy WalkSouthWest)
    ]
