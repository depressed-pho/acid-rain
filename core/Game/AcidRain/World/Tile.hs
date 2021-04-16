{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile
  ( Tile(..)
  , TileID
  , TileState(..)
  , TileStateValue
  , SomeTileState
  , defaultState
  , isSolid
  , isLiquid
  ) where

import Data.Poly.Strict (Poly(..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Data.Word (Word32)
import Game.AcidRain.TUI (Appearance, HasAppearance(..))
import Game.AcidRain.World.Position (WorldPos)
import Prelude.Unicode ((∘))


type TileID = Text

-- | A type in this class defines a tile in the game. A tile at a
-- specific location is represented as a pair of 'Tile' and a single
-- integer value 'TileStateValue', and the values of 'Tile' types are
-- shared across the entire world. For this reason, a type in 'Tile'
-- typically has no values, and instead implements the class on
-- 'Data.Proxy.Proxy' like:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- data Dirt
-- instance 'Tile' ('Data.Proxy.Proxy' Dirt) where
--   tileID _ = "acid-rain:dirt"
--   ..
-- @
class (Show τ, Typeable τ) ⇒ Tile τ where
  -- | Erase the type of the tile.
  upcastTile ∷ τ → Poly Tile
  upcastTile = Poly
  -- | Recover the type of the tile.
  downcastTile ∷ Poly Tile → Maybe τ
  downcastTile (Poly t) = cast t
  -- | Get the tile ID such as @acid-rain:dirt@.
  tileID ∷ τ → TileID
  -- | Get the default state value of the tile.
  defaultStateValue ∷ τ → TileStateValue
  defaultStateValue _ = 0
  -- | Return 'True' iff the tile in a given state prevents entities
  -- from entering the spot where the tile exists.
  isSolidAt ∷ τ → TileStateValue → Bool
  -- | Return 'True' iff the tile in a given state counts as liquid.
  isLiquidAt ∷ τ → TileStateValue → Bool
  -- | Get the appearance of the tile for the given state and
  -- position.
  appearanceAt ∷ τ → TileStateValue → WorldPos → Appearance

instance Show (Poly Tile) where
  showsPrec d (Poly t) = showsPrec d t

instance Tile (Poly Tile) where
  upcastTile = id
  downcastTile = Just
  tileID (Poly t) = tileID t
  defaultStateValue (Poly t) = defaultStateValue t
  isSolidAt (Poly τ) = isSolidAt τ
  isLiquidAt (Poly τ) = isLiquidAt τ
  appearanceAt (Poly t) = appearanceAt t

-- | TileState is a type containing a 'Tile' and a single integral
-- state value. The interpretation of the state value depends on the
-- corresponding tile.
data TileState τ where
  TileState ∷ Tile τ ⇒
    { tsTile  ∷ !τ
    , tsValue ∷ {-# UNPACK #-} !TileStateValue
    } → TileState τ

instance Show τ ⇒ Show (TileState τ) where
  showsPrec d ts
    = showParen (d > appPrec) $
      showString "TileState " ∘
      showString "{ tsTile = "  ∘ showsPrec (appPrec + 1) (tsTile  ts) ∘
      showString ", tsValue = " ∘ showsPrec (appPrec + 1) (tsValue ts) ∘
      showString "}"
    where
      appPrec = 10

instance Tile τ ⇒ HasAppearance (TileState τ, WorldPos) where
  appearance (ts, pos)
    = appearanceAt (tsTile ts) (tsValue ts) pos

-- | A type-erased version of 'TileState'.
type SomeTileState = TileState (Poly Tile)

defaultState ∷ Tile τ ⇒ τ → TileState τ
defaultState t
  = TileState
    { tsTile  = t
    , tsValue = defaultStateValue t
    }

type TileStateValue = Word32

isSolid ∷ TileState τ → Bool
isSolid TileState { tsTile, tsValue }
  = isSolidAt tsTile tsValue

isLiquid ∷ TileState τ → Bool
isLiquid TileState { tsTile, tsValue }
  = isLiquidAt tsTile tsValue
