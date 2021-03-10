{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile
    ( Tile(..)
    , TileState(..)
    , TileStateValue
    )
    where

import Data.Text (Text)
import Data.Word (Word32)
import Game.AcidRain.TUI (Appearance)


class Tile τ where
    -- | Get the tile ID such as @acid-rain:dirt@.
    id ∷ τ → Text
    -- | Get the default state value of the tile.
    defaultStateValue ∷ τ → TileStateValue
    defaultStateValue _ = 0
    -- | Get the appearance of the tile for the given state.
    appearance ∷ τ → TileStateValue → Appearance


-- | TileState is a type containing a type-erased 'Tile' and a single
-- integral state value. The interpretation of the state value depends
-- on the corresponding tile.
data TileState = ∀τ. Tile τ ⇒ TileState
    { tile  ∷ τ
    , value ∷ TileStateValue
    }


type TileStateValue = Word32
