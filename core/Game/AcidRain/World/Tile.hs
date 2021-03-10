{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile
  ( Tile(..)
  , SomeTile(..)
  , TileState(..)
  , TileStateValue
  ) where

import Data.Text (Text)
import Data.Word (Word32)
import Game.AcidRain.TUI (Appearance)


class Tile τ where
    -- | Get the tile ID such as @acid-rain:dirt@.
    tileID ∷ τ → Text
    -- | Get the default state value of the tile.
    defaultStateValue ∷ τ → TileStateValue
    defaultStateValue _ = 0
    -- | Get the appearance of the tile for the given state.
    appearance ∷ τ → TileStateValue → Appearance

-- | A type-erased 'Tile'.
data SomeTile = ∀τ. Tile τ ⇒ SomeTile τ

-- | TileState is a type containing a type-erased 'Tile' and a single
-- integral state value. The interpretation of the state value depends
-- on the corresponding tile.
data TileState = TileState
    { tile  ∷ !SomeTile
    , value ∷ !TileStateValue
    }

type TileStateValue = Word32
