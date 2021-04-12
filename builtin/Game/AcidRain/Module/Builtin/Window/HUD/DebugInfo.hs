{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Window.HUD.DebugInfo
  ( DebugInfo
  , debugInfo
  ) where

import Brick.Types (EventM, Widget, Location(..))
import Brick.Widgets.Core (Named(..), emptyWidget, txt, hBox, vBox, translateBy)
import Control.Exception (Handler(..), catches)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Convertible.Base (convert)
import Data.Monoid.Unicode ((⊕))
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText, fromText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Unique (Unique, newUnique)
import Game.AcidRain.TUI.Window (Window(..), WindowType(..))
import Game.AcidRain.World (World(..), SomeWorld, WorldNotRunningException)
import Game.AcidRain.World.Biome (Biome(..))
import Game.AcidRain.World.Chunk (biomeAt)
import Game.AcidRain.World.Player (PlayerID, plPos)
import Game.AcidRain.World.Position (WorldPos, wpX, wpY, wpZ)
import Lens.Micro ((&), (^.), (.~), to)
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((∘))


data DebugInfo
  = DebugInfo
    { _diName   ∷ !Unique
    , _diWorld  ∷ !SomeWorld
    , _diPlayer ∷ !PlayerID
    , _diWidget ∷ !(Widget Unique)
    }

makeLenses ''DebugInfo

instance Named DebugInfo Unique where
  getName = (^.diName)

instance Window DebugInfo where
  windowID   _ = "acid-rain:debug-info"
  windowType _ = HUD
  renderWindow = (^.diWidget.to pure)
  windowStartEvent di   = redrawDebugInfo di
  handleWorldEvent di _ = redrawDebugInfo di

redrawDebugInfo ∷ DebugInfo → EventM Unique DebugInfo
redrawDebugInfo di
  = liftIO $
    -- Now we are in the IO monad.
    flip catches [ Handler (catchWNRE di)
                 ] $
    do pl ← getPlayerFromWorld (di^.diWorld) (di^.diPlayer)
       mc ← lookupChunk (di^.diWorld) (convert (pl^.plPos))
       case mc of
         Nothing →
           -- The chunk is not available yet. We can only show the
           -- player position in this case.
           return $ di & diWidget .~ renderPos (pl^.plPos)

         Just c →
           do let off = convert (pl^.plPos)
              biome ← biomeAt off c
              let w = vBox
                      [ columns
                        [ renderPos (pl^.plPos)
                        , renderBiome biome
                        ]
                      ]
              return $ di & diWidget .~ w

catchWNRE ∷ DebugInfo → WorldNotRunningException → IO DebugInfo
catchWNRE di _ = return di

withMargin ∷ Widget n → Widget n
withMargin = translateBy (Location (1, 0))

columns ∷ [Widget n] → Widget n
columns = hBox ∘ go True
  where
    go _     []     = []
    go True  (w:ws) =            w : go False ws
    go False (w:ws) = withMargin w : go False ws

renderPos ∷ WorldPos → Widget n
renderPos wp
  = let b = "("
            ⊕ decimal (wp^.wpX) ⊕ ", "
            ⊕ decimal (wp^.wpY) ⊕ ", "
            ⊕ decimal (wp^.wpZ) ⊕ ")"
    in
      txt $ toStrict $ toLazyText b

renderBiome ∷ Biome β ⇒ β → Widget n
renderBiome biome
  = let b = "b=" ⊕ fromText (biomeID biome)
    in
      txt $ toStrict $ toLazyText b

debugInfo ∷ (World w, MonadIO μ) ⇒ w → PlayerID → μ DebugInfo
debugInfo w pid
  = do n ← liftIO $ newUnique
       return DebugInfo
         { _diName   = n
         , _diWorld  = upcastWorld w
         , _diPlayer = pid
         , _diWidget = emptyWidget
         }
