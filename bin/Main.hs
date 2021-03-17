{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (
  App(..), neverShowCursor, invalidateCacheEntry, continue, halt, customMain )
import Brick.Types (BrickEvent(..), Widget, EventM, Next)
import Brick.Widgets.Core (getName)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.UUID (nil)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module (Module(..))
import Game.AcidRain.Module.Builtin (BuiltinModule)
import Game.AcidRain.TUI.Widgets.WorldView (WorldView, worldView, renderWorldView)
import Game.AcidRain.World (World(..), WorldMode(..){-, WorldStateChanged-})
import Game.AcidRain.World.Event
  ( Event, EventDispatcher, EmptyConst, dispatcher, {-addHandler, -}dispatch )
import Game.AcidRain.World.Local (newWorld)
import qualified Graphics.Vty as V

data Name = TheWorldView
  deriving (Show, Eq, Ord)

data AppEvent n
  = Redraw !n
  deriving Show

main ∷ IO ()
main
  = do lw ← newWorld SinglePlayer [upcastModule (Proxy ∷ Proxy BuiltinModule)]

       evChan ← newBChan 256
       let wv = worldView TheWorldView True lw nil
       void $ forkIO $ handleWorldEvents (getName wv) lw evChan

       let buildVty = V.mkVty V.defaultConfig
       initialVty ← buildVty
       void $ customMain initialVty buildVty (Just evChan) theApp wv

handleWorldEvents ∷ World w ⇒ n → w → BChan (AppEvent n) → IO ()
handleWorldEvents n w evChan
  = do e' ← waitForEvent w
       case e' of
         Just e  → dispatch ed e *> handleWorldEvents n w evChan
         Nothing → return ()
  where
    ed ∷ EventDispatcher EmptyConst IO ()
    ed = --addHandler catchWSC $
         dispatcher catchAll

{-
    catchWSC ∷ WorldStateChanged → IO ()
    catchWSC _ = writeBChan evChan $ Redraw n
-}

    -- In theory we could be listening on all the events that can
    -- possibly outdate the world view and be doing nothing in the
    -- fallback handler, but that is very error-prone. Instead we do
    -- the opposite. We listen on individual events that don't need to
    -- redraw the view.
    catchAll ∷ ∀e. Event e ⇒ e → IO ()
    catchAll _ = writeBChan evChan $ Redraw n

theApp ∷ App (WorldView Name) (AppEvent Name) Name
theApp = App
         { appDraw         = drawUI
         , appChooseCursor = neverShowCursor
         , appHandleEvent  = appEvent
         , appStartEvent   = return
         , appAttrMap      = const $ A.attrMap V.defAttr []
         }

drawUI ∷ Ord n ⇒ WorldView n → [Widget n]
drawUI wv = [renderWorldView wv]

appEvent ∷ Ord n ⇒ WorldView n → BrickEvent n (AppEvent n) → EventM n (Next (WorldView n))
appEvent wv (VtyEvent (V.EvResize _ _)) = continue wv
appEvent wv (AppEvent (Redraw n)) = invalidateCacheEntry n *> continue wv
appEvent wv _ = halt wv
