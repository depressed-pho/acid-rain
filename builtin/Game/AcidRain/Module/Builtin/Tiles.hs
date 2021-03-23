{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Tiles
  ( loadTiles
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module.Loader (LoaderContext, registerTile)
import Game.AcidRain.TUI (Appearance(..), begin, end, (⊳), unicode, ascii, fgColour, hsl)
import Game.AcidRain.World.Position (wpZ, lowestZ)
import Game.AcidRain.World.Tile (Tile(..))
import Lens.Micro ((^.))
import Prelude.Unicode ((≡))


data Air
instance Tile (Proxy Air) where
  tileID _ = "acid-rain:air"
  appearanceFor _ _ _ = InvisibleAppearance

data Dirt
instance Tile (Proxy Dirt) where
  tileID _ = "acid-rain:dirt"
  appearanceFor _ _ pos
    | pos^.wpZ ≡ lowestZ = begin ⊳ unicode "." ⊳ ascii '.' ⊳ fgColour colour ⊳ end
    | otherwise          = begin ⊳ unicode "#" ⊳ ascii '#' ⊳ fgColour colour ⊳ end
    where
      colour = hsl 43.0 0.89 0.38 -- CSS3 darkgoldenrod

data Water
instance Tile (Proxy Water) where
  tileID _ = "acid-rain:water"
  appearanceFor _ _ _ = begin ⊳ unicode "~" ⊳ ascii '~' ⊳ fgColour colour ⊳ end
    where
      colour = hsl 195.0 1.0 0.5 -- CSS3 deepskyblue

loadTiles ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadTiles
  = traverse_ registerTile
    [ upcastTile (Proxy ∷ Proxy Air)
    , upcastTile (Proxy ∷ Proxy Dirt)
    , upcastTile (Proxy ∷ Proxy Water)
    ]
