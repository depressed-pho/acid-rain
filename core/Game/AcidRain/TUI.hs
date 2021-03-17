{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI
  ( -- * The appearance type
    Appearance(..)
  , HasAppearance(..)

    -- * Helper for building 'Appearance'
  , AppearanceBuilder
  , begin
  , end
  , (⊳)
  , (|>)

    -- ** Setting fields
  , unicode
  , ascii
  , attr

    -- ** Modifying attributes
  , standout
  , italic
  , strikethrough
  , underline
  , reverseVideo
  , blink
  , dim
  , bold
  , ColourVariant(..)
  , fgColour
  , bgColour

    -- ** Helper functions
  , rgb
  , hsl
  ) where

import Control.Exception (assert)
import Data.Text (Text)
import Data.Colour (Colour)
import qualified Data.Colour.RGBSpace.HSL as HSL
import Data.Colour.SRGB (RGB(..), toSRGB24, sRGB)
import Graphics.Vty.Attributes (Attr, withStyle)
import qualified Graphics.Vty.Attributes as V
import qualified Graphics.Vty.Image as V
import Prelude.Unicode ((≢))


-- | An appearance is an attributed character (styled and colored)
-- that represents a tile, an item, or an entity on a terminal. On
-- Unicode capable terminals it may consist of several code points,
-- but must still form a single grapheme cluster. Its displayed width
-- must be exactly 1.
data Appearance
  = Appearance
    { apUnicode ∷ !Text
    , apAscii   ∷ !Char
    , apAttr    ∷ !Attr
    } deriving (Eq, Show)

-- | Types satisfying 'HasAppearance', obviously, has an appearance.
class HasAppearance α where
  appearance ∷ α → Appearance

-- THINKME: We should probably use Generics or TemplateHaskell rather
-- than writing this builder down by hand.

-- | 'Appearance' can be constructed with the regular record syntax,
-- but to ease constructing appearances this module provides a
-- type-safe record builder that can be used like this:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- ap ∷ 'Appearance'
-- ap = 'begin'
--        '⊳' 'unicode' "."
--        '⊳' 'ascii' \'.\'
--        '⊳' 'fgColour' ('hsl' 36.0 0.89 0.38)
--        '⊳' 'end'
-- @
data AppearanceBuilder unicode ascii attr
  = AppearanceBuilder
    { abUnicode ∷ !unicode
    , abAscii   ∷ !ascii
    , abAttr    ∷ !attr
    }

class OptionalField base α where
  fromOptional ∷ α → base

instance OptionalField Attr Attr where
  fromOptional = id

instance OptionalField Attr () where
  fromOptional _ = V.currentAttr

-- | Declare the beginning of appearance builder.
begin ∷ AppearanceBuilder () () ()
begin
  = AppearanceBuilder
    { abUnicode = ()
    , abAscii   = ()
    , abAttr    = ()
    }

-- | Declare the end of appearance builder.
end ∷ OptionalField Attr attr ⇒ AppearanceBuilder Text Char attr → Appearance
end ab
  = Appearance
    { apUnicode = abUnicode ab
    , apAscii   = abAscii ab
    , apAttr    = fromOptional $ abAttr ab
    }

-- | Composition operator for appearance builder. This is defined as
-- @'flip' ('$')@.
(⊳) ∷ a → (a → b) → b
(⊳) = flip ($)
infixl 0 ⊳

-- | An alias to '(⊳)' if you don't like the unicode symbol.
(|>) ∷ a → (a → b) → b
(|>) = (⊳)
infixl 0 |>

-- | Declare a value for the field 'apUnicode'. This must appear
-- exactly once in a builder, or it won't typecheck. Throws if the
-- displayed width of the text is not exactly 1.
unicode ∷ Text → AppearanceBuilder () ascii attr → AppearanceBuilder Text ascii attr
unicode a ab
  = assert (V.safeWctwidth a ≢ 1) $
    ab { abUnicode = a }

-- | Declare a value for the field 'apAscii'. This must appear exactly
-- once in a builder, or it won't typecheck. Throws if the displayed
-- width of the character is not exactly 1.
ascii ∷ Char → AppearanceBuilder unicode () attr → AppearanceBuilder unicode Char attr
ascii a ab
  = assert (V.safeWcwidth a ≢ 1) $
    ab { abAscii = a }

-- | Declare a value for the field 'apAttr'. This is optional and is
-- defaulted to 'V.currentAttr'. But if it exists, it must not occur
-- after any field constructors declaring a value for the field.
attr ∷ Attr → AppearanceBuilder unicode ascii () → AppearanceBuilder unicode ascii Attr
attr a ab = ab { abAttr = a }

-- | Apply a style to the attributes. All of these are optional and
-- can occur as many times as needed.
standout, italic, strikethrough, underline, reverseVideo, blink, dim, bold
  ∷ OptionalField Attr attr
  ⇒ AppearanceBuilder unicode ascii attr
  → AppearanceBuilder unicode ascii Attr
standout      ab = ab { abAttr = (fromOptional $ abAttr ab) `withStyle` V.standout      }
italic        ab = ab { abAttr = (fromOptional $ abAttr ab) `withStyle` V.italic        }
strikethrough ab = ab { abAttr = (fromOptional $ abAttr ab) `withStyle` V.strikethrough }
underline     ab = ab { abAttr = (fromOptional $ abAttr ab) `withStyle` V.underline     }
reverseVideo  ab = ab { abAttr = (fromOptional $ abAttr ab) `withStyle` V.reverseVideo  }
blink         ab = ab { abAttr = (fromOptional $ abAttr ab) `withStyle` V.blink         }
dim           ab = ab { abAttr = (fromOptional $ abAttr ab) `withStyle` V.dim           }
bold          ab = ab { abAttr = (fromOptional $ abAttr ab) `withStyle` V.bold          }

-- | This is a class to accept any types that can be converted to
-- 'V.Color'.
class ColourVariant α where
  toVtyColour ∷ α → V.Color

instance ColourVariant V.Color where
  toVtyColour = id

instance (Floating e, RealFrac e) ⇒ ColourVariant (Colour e) where
  -- I wanted to use the Color package but it was RIDICULOUSLY HARD TO
  -- USE. IT WAS FUCKING HARD TO USE. I HAD A FUCKING SERIOUSLY HARD
  -- TIME TO FIND OUT HOW THE FUCK TO DO THIS. AND I GAVE UP IN THE
  -- END.
  toVtyColour c
    = case toSRGB24 c of
        RGB r g b → V.rgbColor r g b

-- | Set the foreground or background color of the attributes. Both of
-- these are optional.
fgColour, bgColour
  ∷ (OptionalField Attr attr, ColourVariant α)
  ⇒ α
  → AppearanceBuilder unicode ascii attr
  → AppearanceBuilder unicode ascii Attr
fgColour c ab = ab { abAttr = (fromOptional $ abAttr ab) `V.withForeColor` toVtyColour c }
bgColour c ab = ab { abAttr = (fromOptional $ abAttr ab) `V.withBackColor` toVtyColour c }

-- Is this really a correct way to convert RGB to Colour…?
sRGBToColour ∷ (Floating e, Ord e) ⇒ RGB e → Colour e
sRGBToColour (RGB r g b) = sRGB r g b

-- | Construct a colour by specifying red @[0, 1]@, green @[0, 1]@,
-- and blue @[0, 1]@ components in the sRGB colour space.
rgb ∷ Float → Float → Float → Colour Float
rgb = sRGB

-- | Construct a colour by specifying hue @[0, 360]@, saturation @[0,
-- 1]@, and lightness @[0, 1]@ in the sRGB colour space.
hsl ∷ Float → Float → Float → Colour Float
hsl r g b = sRGBToColour $ HSL.hsl r g b
