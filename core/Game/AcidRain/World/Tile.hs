{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile
  ( Tile(..)
  , TileID
  , SomeTile(..)
  , TileState(..)
  , TileStateValue
  ) where

import Data.Text (Text)
import Data.Word (Word32)
import Game.AcidRain.TUI (Appearance)

type TileID = Text

class Tile τ where
    -- | Get the tile ID such as @acid-rain:dirt@.
    tileID ∷ τ → TileID
    -- | Get the default state value of the tile.
    defaultStateValue ∷ τ → TileStateValue
    defaultStateValue _ = 0
    -- | Get the appearance of the tile for the given state.
    appearance ∷ τ → TileStateValue → Appearance

-- | A type-erased 'Tile'.
data SomeTile = ∀τ. Tile τ ⇒ SomeTile τ

instance Tile SomeTile where
  tileID (SomeTile t) = tileID t
  defaultStateValue (SomeTile t) = defaultStateValue t
  appearance (SomeTile t) = appearance t

-- | TileState is a type containing a type-erased 'Tile' and a single
-- integral state value. The interpretation of the state value depends
-- on the corresponding tile.
data TileState = TileState
    { tsTile  ∷ !SomeTile
    , tsValue ∷ !TileStateValue
    }

type TileStateValue = Word32
