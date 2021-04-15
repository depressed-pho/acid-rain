{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import qualified Brick.AttrMap as A
import Brick.BChan (newBChan)
import Brick.Main (
  App(..), neverShowCursor, continue, halt, customMain )
import Brick.Types (BrickEvent(..), Widget, EventM, Next)
import Control.Monad (void)
import Data.Poly.Strict (Poly)
import Data.Proxy (Proxy(..))
import Data.UUID (nil)
import Data.Unique (Unique)
import GHC.Conc (setNumCapabilities, getNumProcessors)
import Game.AcidRain.Module (Module(..))
import Game.AcidRain.Module.Builtin (BuiltinModule)
import Game.AcidRain.TUI.AppEvent (AppEvent)
import Game.AcidRain.TUI.Client
  ( Client, newClient, drawClient, handleClientEvent, isClientClosed )
import Game.AcidRain.World (WorldMode(..))
import Game.AcidRain.World.Local (newWorld)
import qualified Graphics.Vty as V

main ∷ IO ()
main
  = do numProc ← getNumProcessors
       setNumCapabilities $ max 1 (numProc - 1)
       setNumCapabilities 3 -- FIXME: remove this

       let seed = 666
       lw ← newWorld SinglePlayer [upcastModule (Proxy ∷ Proxy BuiltinModule)] seed

       evChan ← newBChan 256
       cli    ← newClient True lw nil evChan

       let buildVty = V.mkVty V.defaultConfig
       initialVty ← buildVty
       void $ customMain initialVty buildVty (Just evChan) theApp cli

theApp ∷ App Client (Poly AppEvent) Unique
theApp = App
         { appDraw         = drawUI
         , appChooseCursor = neverShowCursor
         , appHandleEvent  = appEvent
         , appStartEvent   = return
         , appAttrMap      = const $ A.attrMap V.defAttr []
         }

drawUI ∷ Client → [Widget Unique]
drawUI = drawClient

appEvent ∷ Client → BrickEvent Unique (Poly AppEvent) → EventM Unique (Next Client)
appEvent cli be
  = do cli' ← handleClientEvent cli be
       if isClientClosed cli'
         then halt cli'
         else continue cli'
