{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Loader.Tile
  ( loadTiles
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Game.AcidRain.Module.Loader (LoaderContext, registerTile)
import Game.AcidRain.TUI (begin, end, (⊳), unicode, ascii, fgColour, hsl)
import Game.AcidRain.World.Tile (Tile(..))


data Dirt
instance Tile (Proxy Dirt) where
  tileID     _   = "acid-rain:dirt"
  appearance _ _ = begin ⊳ unicode "." ⊳ ascii '.' ⊳ fgColour (hsl 43.0 0.89 0.38) ⊳ end
  -- The color is CSS3 darkgoldenrod

loadTiles ∷ (MonadState LoaderContext μ, MonadThrow μ) ⇒ μ ()
loadTiles
  = traverse_ registerTile
    [ upcastTile (Proxy ∷ Proxy Dirt)
    ]
