{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI
  ( Appearance(..)
  ) where

import Data.Text (Text)
import Graphics.Vty.Attributes (Attr)

-- | An appearance is an attributed character (styled and colored)
-- that represents a tile, an item, or an entity on a terminal. On
-- Unicode capable terminals it may consist of several code points,
-- but must still form a single grapheme cluster. Its displayed width
-- must be exactly 1.
data Appearance = Appearance
    { unicode ∷ !Text
    , ascii   ∷ !Char
    , attr    ∷ !Attr
    } deriving (Eq, Show)
