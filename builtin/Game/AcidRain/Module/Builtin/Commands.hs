{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Commands
  ( loadCommands
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module.Builtin.Window.HUD.DebugInfo (debugInfo)
import Game.AcidRain.Module.Builtin.Commands.TH (mkWalkCmd)
import Game.AcidRain.Module.Loader (LoaderContext, registerCommand)
import Game.AcidRain.TUI.Keystroke (keyQ)
import Game.AcidRain.TUI.Window (Window(..))
import Game.AcidRain.World
  ( Command(..), CommandType(..), BadArgumentsException(..)
  , getClientWorld, getClientPlayerID
  , hasWindow, insertWindow, deleteWindow
  , ClientOnlyCommandException(..), throwSomeExc )
import Game.AcidRain.World.Position (wpX, wpY)
import Lens.Micro ((-~), (+~))
import Prelude.Unicode ((∘))


-------------------------------------------------------------------------------
-- Walk

mkWalkCmd "WalkNorth"     "acid-rain:walk-north"      [keyQ|k|] [| wpY -~ 1 |]
mkWalkCmd "WalkWest"      "acid-rain:walk-west"       [keyQ|h|] [| wpX -~ 1 |]
mkWalkCmd "WalkEast"      "acid-rain:walk-east"       [keyQ|l|] [| wpX +~ 1 |]
mkWalkCmd "WalkSouth"     "acid-rain:walk-south"      [keyQ|j|] [| wpY +~ 1 |]
mkWalkCmd "WalkNorthWest" "acid-rain:walk-north-west" [keyQ|y|] [| (wpX -~ 1) ∘ (wpY -~ 1) |]
mkWalkCmd "WalkNorthEast" "acid-rain:walk-north-east" [keyQ|u|] [| (wpX +~ 1) ∘ (wpY -~ 1) |]
mkWalkCmd "WalkSouthEast" "acid-rain:walk-south-east" [keyQ|n|] [| (wpX +~ 1) ∘ (wpY +~ 1) |]
mkWalkCmd "WalkSouthWest" "acid-rain:walk-south-west" [keyQ|b|] [| (wpX -~ 1) ∘ (wpY +~ 1) |]

-------------------------------------------------------------------------------
-- Run

-- FIXME: We should be doing this by writing a "running state" into
-- 'Entity' and evaluating it while ticking entities.

mkWalkCmd "RunNorth"     "acid-rain:run-north"      [keyQ|K|] [| wpY -~ 16 |]
mkWalkCmd "RunWest"      "acid-rain:run-west"       [keyQ|H|] [| wpX -~ 16 |]
mkWalkCmd "RunEast"      "acid-rain:run-east"       [keyQ|L|] [| wpX +~ 16 |]
mkWalkCmd "RunSouth"     "acid-rain:run-south"      [keyQ|J|] [| wpY +~ 16 |]
mkWalkCmd "RunNorthWest" "acid-rain:run-north-west" [keyQ|Y|] [| (wpX -~ 16) ∘ (wpY -~ 16) |]
mkWalkCmd "RunNorthEast" "acid-rain:run-north-east" [keyQ|U|] [| (wpX +~ 16) ∘ (wpY -~ 16) |]
mkWalkCmd "RunSouthEast" "acid-rain:run-south-east" [keyQ|N|] [| (wpX +~ 16) ∘ (wpY +~ 16) |]
mkWalkCmd "RunSouthWest" "acid-rain:run-south-west" [keyQ|B|] [| (wpX -~ 16) ∘ (wpY +~ 16) |]

-------------------------------------------------------------------------------
-- Misc
data ToggleDebug
instance Command (Proxy ToggleDebug) where
  commandID   _ = "acid-rain:toggle-debug"
  commandType _ = Interactive $ Just [keyQ|<f3>|]
  runOnClient _ []
    = do w   ← getClientWorld
         pid ← getClientPlayerID
         dbg ← debugInfo w pid
         h   ← hasWindow (windowID dbg)
         if h
           then deleteWindow (windowID dbg)
           else insertWindow dbg
  runOnClient _ _
    = throwSomeExc BadArgumentsException
  runOnWorld _ _ _ = throwSomeExc ClientOnlyCommandException

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

    , upcastCommand (Proxy ∷ Proxy RunNorth)
    , upcastCommand (Proxy ∷ Proxy RunWest)
    , upcastCommand (Proxy ∷ Proxy RunEast)
    , upcastCommand (Proxy ∷ Proxy RunSouth)
    , upcastCommand (Proxy ∷ Proxy RunNorthWest)
    , upcastCommand (Proxy ∷ Proxy RunNorthEast)
    , upcastCommand (Proxy ∷ Proxy RunSouthEast)
    , upcastCommand (Proxy ∷ Proxy RunSouthWest)

    , upcastCommand (Proxy ∷ Proxy ToggleDebug)
    ]
