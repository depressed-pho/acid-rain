{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import qualified Brick.AttrMap as A
import Brick.Main (App(..), neverShowCursor, continue, halt, customMain)
import Brick.Types (BrickEvent(..), Widget, EventM, Next)
import Control.Monad (forever, void)
import Data.UUID (nil)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module (Module(..))
import Game.AcidRain.Module.Builtin (BuiltinModule)
import Game.AcidRain.TUI.Widgets.WorldView (WorldView, worldView, renderWorldView)
import Game.AcidRain.World (World(..), WorldMode(..))
import Game.AcidRain.World.Chunk.Position (ChunkPos(..))
import Game.AcidRain.World.Local (newWorld)
import qualified Graphics.Vty as V

data Name = Name1
  deriving (Show, Eq, Ord)

main ∷ IO ()
main
  = do lw ← newWorld SinglePlayer [upcastModule (Proxy ∷ Proxy BuiltinModule)]
       let wv = worldView Name1 True lw nil

       let buildVty = V.mkVty V.defaultConfig
       initialVty ← buildVty
       void $ customMain initialVty buildVty Nothing theApp wv

       return ()
       {-
       forever $
         do e ← waitForEvent lw
            putStrLn (show e)
            ensureChunkExists lw (ChunkPos 0 0)
-}

theApp ∷ App (WorldView Name) e Name
theApp = App
         { appDraw         = drawUI
         , appChooseCursor = neverShowCursor
         , appHandleEvent  = appEvent
         , appStartEvent   = return
         , appAttrMap      = const $ A.attrMap V.defAttr []
         }

drawUI ∷ WorldView n → [Widget n]
drawUI wv = [renderWorldView wv]

appEvent ∷ WorldView n → BrickEvent n e → EventM n (Next (WorldView n))
appEvent wv (VtyEvent (V.EvResize _ _)) = continue wv
appEvent wv _ = halt wv
