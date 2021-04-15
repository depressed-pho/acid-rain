{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.AppEvent
  ( AppEvent(..)
  ) where

import Data.Poly.Strict (Poly(..))
import Data.Typeable (Typeable, cast)


-- | Application event for Brick.
class Typeable ε ⇒ AppEvent ε where
  -- | Erase the type of the application event.
  upcastAppEvent ∷ ε → Poly AppEvent
  upcastAppEvent = Poly
  -- | Recover the type of the application event.
  downcastAppEvent ∷ Poly AppEvent → Maybe ε
  downcastAppEvent (Poly e) = cast e

instance AppEvent (Poly AppEvent) where
  upcastAppEvent = id
  downcastAppEvent = Just
