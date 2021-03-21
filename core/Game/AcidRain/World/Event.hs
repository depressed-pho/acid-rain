{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Event
  ( -- * The event class
    Event(..)
  , SomeEvent(..)

    -- * Dispatching events
  , EventDispatcher
  , EventHandler
  , dispatcher
  , addHandler
  , dispatch
  ) where

import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import Data.TypeRepMap (TypeRepMap)
import qualified Data.TypeRepMap as TRM
import Lens.Micro ((&), (^.), (%~))
import Lens.Micro.TH (makeLenses)


class (Show e, Typeable e) ⇒ Event e where
  -- | Erase the type of the event.
  upcastEvent ∷ e → SomeEvent
  upcastEvent = SomeEvent

-- | A type-erased 'Event'.
data SomeEvent = ∀e. Event e ⇒ SomeEvent !e

instance Show SomeEvent where
  showsPrec d (SomeEvent e) = showsPrec d e

instance Event SomeEvent where
  upcastEvent = id

-- | @'EventHandler' e c f r@ is a type alias to @('Event' e, c) ⇒ e →
--   f r@, which is a function that takes an 'Event' @e@ and returns
--   @r@ on a context @f@ where @c@ holds. The context @f@ needs to be
--   an 'Applicative'.
type EventHandler (e ∷ Type) (c ∷ Constraint) (f ∷ * → *) r
  = (Event e, c, Applicative f) ⇒ e → f r

-- This is a workaround for a limitation in GHC where type functions
-- can't to be partially applied and therefore can't parameterise
-- TypeRepMap:
-- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0242-unsaturated-type-families.rst
newtype EH (c ∷ Constraint) (f ∷ * → *) r (e ∷ Type)
  = EH (EventHandler e c f r)

-- | This is a heterogeneous set of 'EventHandler's, each taking some
-- specific type of 'Event' and does some action.
--
-- * The type parameter @c@ is some constraint on which actions
--   occur. For example, if your actions occur on
--   @'Control.Monad.IO.Class.MonadIO' m@ the parameter @c@ would be
--   @'Control.Monad.IO.Class.MonadIO' m@. You can use @()@ if your
--   context is has no constraints.
--
-- * The type parameter @f@ is the context on which actions occur. It
--   needs to be an 'Applicative'.
--
-- * The type parameter @r@ is the return value of actions.
--
-- These parameters determine the type of event handlers. For example,
--
-- * @'EventDispatcher' ('Control.Monad.IO.Class.MonadIO' m) m ()@
--   expects event handlers of type @'Control.Monad.IO.Class.MonadIO'
--   m ⇒ e → m ()@ for some 'Event' @e@.
--
-- * @'EventDispatcher' () 'IO' ()@ expects event handlers of type @e
--   → 'IO' ()@ for some 'Event' @e@.
data EventDispatcher (c ∷ Constraint) (f ∷ * → *) r
  = EventDispatcher
    { _handlers ∷ !(TypeRepMap (EH c f r))
    , _fallback ∷ !(∀e. EventHandler e c f r)
    }
makeLenses ''EventDispatcher

-- | @'dispatcher' fb@ creates an empty dispatcher with a fallback
-- handler @fb@ which is a rank-2 polymorphic function taking any type
-- of 'Event' and runs an action.
dispatcher ∷ (∀e. (Event e, (c ∷ Constraint), Applicative f) ⇒ EventHandler e c f r)
           → EventDispatcher c f r
dispatcher fb
  = EventDispatcher
    { _handlers = TRM.empty
    , _fallback = fb
    }

-- | @'addHandler' h ed@ registers an event handler @h@ to the
-- dispatcher @ed@. For example, the following code registers a
-- handler for 'Game.AcidRain.World.WorldStateChanged':
--
-- @
-- handleWSC ∷ 'Game.AcidRain.World.WorldStateChanged' → 'IO' ()
-- handleWSC _ = 'fail' "World state changed"
-- ..
-- 'addHandler' handleWSC ed -- where ed ∷ 'EventDispatcher' () 'IO' ()
-- @
--
-- The type of the handler function determines the type of 'Event' it
-- handles. If there is already a handler for the same event type, the
-- new one will run after the old one, discarding the return value of
-- the old one.
addHandler ∷ (Event e, (c ∷ Constraint), Applicative f)
           ⇒ EventHandler e c f r
           → EventDispatcher c f r
           → EventDispatcher c f r
addHandler h ed
  = case TRM.lookup (ed^.handlers) of
      Just (EH h') → ed & handlers %~ TRM.insert (EH (h' *> h))
      Nothing      → ed & handlers %~ TRM.insert (EH h)

-- | @'dispatch' ed e@ invokes an event handler registered for the
-- event type of @e@ if there is one. Otherwise it invokes the
-- fallback handler.
dispatch ∷ (c ∷ Constraint, Applicative f) ⇒ EventDispatcher c f r → SomeEvent → f r
dispatch ed (SomeEvent e)
  = case TRM.lookup (ed^.handlers) of
      Just (EH h) → h e
      Nothing     → (ed^.fallback) e
