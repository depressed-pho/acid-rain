{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Event
  ( -- * The event class
    Event(..)
  , SomeEvent(..)
  ) where


class Show α ⇒ Event α where
  -- | Erase the type of the event.
  upcastEvent ∷ α → SomeEvent
  upcastEvent = SomeEvent

-- | A type-erased 'Event'.
data SomeEvent = ∀α. Event α ⇒ SomeEvent α

instance Show SomeEvent where
  showsPrec d (SomeEvent e) = showsPrec d e

instance Event SomeEvent where
  upcastEvent = id
