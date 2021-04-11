{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.TUI.AppEvent
  ( AppEvent(..)
  , SomeAppEvent(..)
  ) where

import Data.Typeable (Typeable, cast)


-- | Application event for Brick.
class Typeable ε ⇒ AppEvent ε where
  -- | Erase the type of the application event.
  upcastAppEvent ∷ ε → SomeAppEvent
  upcastAppEvent = SomeAppEvent
  -- | Recover the type of the application event.
  downcastAppEvent ∷ SomeAppEvent → Maybe ε
  downcastAppEvent (SomeAppEvent e) = cast e

-- | A type-erased 'AppEvent', suitable for passing from\/to Brick.
data SomeAppEvent = ∀ε. AppEvent ε ⇒ SomeAppEvent !ε

instance AppEvent SomeAppEvent where
  upcastAppEvent = id
  downcastAppEvent = Just
