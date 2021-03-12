{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile
  ( Tile(..)
  , TileID
  , SomeTile(..)
  , TileState(..)
  , TileStateValue
  , SomeTileState
  , defaultState
  ) where

import Data.Text (Text)
import Data.Word (Word32)
import Game.AcidRain.TUI (Appearance)

type TileID = Text

class Show τ ⇒ Tile τ where
  -- | Erase the type of the tile.
  upcastTile ∷ τ → SomeTile
  upcastTile = SomeTile
  -- | Get the tile ID such as @acid-rain:dirt@.
  tileID ∷ τ → TileID
  -- | Get the default state value of the tile.
  defaultStateValue ∷ τ → TileStateValue
  defaultStateValue _ = 0
  -- | Get the appearance of the tile for the given state.
  appearance ∷ τ → TileStateValue → Appearance

-- | A type-erased 'Tile'.
data SomeTile = ∀τ. Tile τ ⇒ SomeTile τ

instance Show SomeTile where
  show (SomeTile t) = show t

instance Tile SomeTile where
  upcastTile = id
  tileID (SomeTile t) = tileID t
  defaultStateValue (SomeTile t) = defaultStateValue t
  appearance (SomeTile t) = appearance t

-- | TileState is a type containing a 'Tile' and a single integral
-- state value. The interpretation of the state value depends on the
-- corresponding tile.
data TileState τ where
  TileState ∷ Tile τ ⇒
    { tsTile  ∷ !τ
    , tsValue ∷ {-# UNPACK #-} !TileStateValue
    } → TileState τ

-- | A type-erased version of 'TileState'.
type SomeTileState = TileState SomeTile

defaultState ∷ Tile τ ⇒ τ → TileState τ
defaultState t
  = TileState
    { tsTile  = t
    , tsValue = defaultStateValue t
    }

type TileStateValue = Word32
