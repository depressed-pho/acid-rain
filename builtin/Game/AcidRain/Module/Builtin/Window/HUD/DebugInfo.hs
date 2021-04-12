{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Window.HUD.DebugInfo
  ( DebugInfo
  , debugInfo
  ) where

import Brick.Types (EventM, Widget, Location(..))
import Brick.Widgets.Core (Named(..), txt, hBox, translateBy)
import Control.Exception (Handler(..), catches)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Convertible.Base (convert)
import Data.Monoid.Unicode ((⊕))
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (FPFormat(..), formatRealFloat)
import Data.Unique (Unique, newUnique)
import Game.AcidRain.TUI.Window (Window(..), WindowType(..))
import Game.AcidRain.World (World(..), SomeWorld, WorldNotRunningException)
import Game.AcidRain.World.Biome (Biome(..))
import Game.AcidRain.World.Chunk (tileStateAt, biomeAt, climateAt)
import Game.AcidRain.World.Climate (Climate(..))
import Game.AcidRain.World.Player (PlayerID, plPos)
import Game.AcidRain.World.Position (WorldPos, wpX, wpY, wpZ)
import Game.AcidRain.World.Tile (Tile(..), TileState(..))
import Lens.Micro ((&), (^.), (.~))
import Lens.Micro.TH (makeLenses)
import Prelude.Unicode ((∘), (⋅))


data DebugInfo
  = DebugInfo
    { _diName    ∷ !Unique
    , _diWorld   ∷ !SomeWorld
    , _diPlayer  ∷ !PlayerID
    , _diWidgets ∷ !(Seq (Widget Unique))
    }

makeLenses ''DebugInfo

instance Named DebugInfo Unique where
  getName = (^.diName)

instance Window DebugInfo where
  windowID   _ = "acid-rain:debug-info"
  windowType _ = HUD
  renderWindow = (^.diWidgets)
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
           return $ di & diWidgets .~ pure (renderPos (pl^.plPos))

         Just c →
           do let off = convert (pl^.plPos)
                  cli = climateAt off c
              ts  ← tileStateAt off c
              bio ← biomeAt     off c
              let ws = rows
                       [ columns
                         [ renderPos (pl^.plPos)
                         , renderTileState ts
                         , renderBiome bio
                         ]
                       , columns
                         [ renderTemp  cli
                         , renderHumid cli
                         , renderAlt   cli
                         ]
                       ]
              return $ di & diWidgets .~ ws

catchWNRE ∷ DebugInfo → WorldNotRunningException → IO DebugInfo
catchWNRE di _ = return di

withMargin ∷ Widget n → Widget n
withMargin = translateBy (Location (1, 0))

rows ∷ [Widget n] → Seq (Widget n)
rows = S.fromList ∘ zipWith f [0..]
  where
    f y = translateBy (Location (0, y))

columns ∷ [Widget n] → Widget n
columns = hBox ∘ go True
  where
    go _     []     = []
    go True  (w:ws) =            w : go False ws
    go False (w:ws) = withMargin w : go False ws

fromBuilder ∷ Builder → Widget n
fromBuilder = txt ∘ toStrict ∘ toLazyText

renderPos ∷ WorldPos → Widget n
renderPos wp
  = fromBuilder $
    "("
    ⊕ decimal (wp^.wpX) ⊕ ", "
    ⊕ decimal (wp^.wpY) ⊕ ", "
    ⊕ decimal (wp^.wpZ) ⊕ ")"

renderTileState ∷ TileState τ → Widget n
renderTileState (TileState { tsTile, tsValue })
  = fromBuilder $
    "tile=" ⊕ fromText (tileID tsTile)
    ⊕ "/"   ⊕ decimal tsValue

renderBiome ∷ Biome β ⇒ β → Widget n
renderBiome biome
  = fromBuilder $
    "bio=" ⊕ fromText (biomeID biome)

renderTemp ∷ Climate → Widget n
renderTemp cli
  = fromBuilder $
    "temp=" ⊕ formatRealFloat Fixed (Just 2) (cliTemperature cli)

renderHumid ∷ Climate → Widget n
renderHumid cli
  = fromBuilder $
    "hum=" ⊕ formatRealFloat Fixed (Just 2) (cliHumidity cli ⋅ 100)

renderAlt ∷ Climate → Widget n
renderAlt cli
  = fromBuilder $
    "alt=" ⊕ formatRealFloat Fixed (Just 2) (cliAltitude cli)

debugInfo ∷ (World w, MonadIO μ) ⇒ w → PlayerID → μ DebugInfo
debugInfo w pid
  = do n ← liftIO $ newUnique
       return DebugInfo
         { _diName    = n
         , _diWorld   = upcastWorld w
         , _diPlayer  = pid
         , _diWidgets = S.empty
         }
