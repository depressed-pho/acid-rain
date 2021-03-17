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
import Game.AcidRain.TUI (Appearance, HasAppearance(..))

type TileID = Text

-- | An instance of this class defines a tile in the game. A tile at a
-- specific location is represented as a pair of 'Tile' and a single
-- integer value 'TileStateValue', and the values of 'Tile' types are
-- shared across the entire world. For this reason, an instance of
-- 'Tile' typically has no values, and instead implements the class on
-- 'Data.Proxy.Proxy' like:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- data Dirt
-- instance 'Tile' ('Data.Proxy.Proxy' Dirt) where
--   tileID _ = "acid-rain:dirt"
--   ..
-- @
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
  appearanceForState ∷ τ → TileStateValue → Appearance

-- | A type-erased 'Tile'.
data SomeTile = ∀τ. Tile τ ⇒ SomeTile !τ

instance Show SomeTile where
  showsPrec d (SomeTile t) = showsPrec d t

instance Tile SomeTile where
  upcastTile = id
  tileID (SomeTile t) = tileID t
  defaultStateValue (SomeTile t) = defaultStateValue t
  appearanceForState (SomeTile t) = appearanceForState t

-- | TileState is a type containing a 'Tile' and a single integral
-- state value. The interpretation of the state value depends on the
-- corresponding tile.
data TileState τ where
  TileState ∷ Tile τ ⇒
    { tsTile  ∷ !τ
    , tsValue ∷ {-# UNPACK #-} !TileStateValue
    } → TileState τ

instance Tile τ ⇒ HasAppearance (TileState τ) where
  appearance ts
    = appearanceForState (tsTile ts) (tsValue ts)

-- | A type-erased version of 'TileState'.
type SomeTileState = TileState SomeTile

defaultState ∷ Tile τ ⇒ τ → TileState τ
defaultState t
  = TileState
    { tsTile  = t
    , tsValue = defaultStateValue t
    }

type TileStateValue = Word32
