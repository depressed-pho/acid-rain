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
  , EmptyConst
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

-- | @'EventHandler' e c m r@ is a type alias to @('Event' e, c m) ⇒ e
--   → m r@, which is a function that takes an 'Event' @e@ and returns
--   @r@ on a context @m@ where @c m@ holds.
type EventHandler (e ∷ Type) (c ∷ (* → *) → Constraint) (m ∷ * → *) r
  = (Event e, c m) ⇒ e → m r

-- This is a workaround for a limitation in GHC where type functions
-- can't to be partially applied and therefore can't parameterise
-- TypeRepMap:
-- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0242-unsaturated-type-families.rst
newtype EH (c ∷ (* → *) → Constraint) (m ∷ * → *) r (e ∷ Type)
  = EH (EventHandler e c m r)

-- | This is a heterogeneous set of 'EventHandler's, each taking some
-- specific type of 'Event' and does some action.
--
-- * The type parameter @c@ is a constraint on the context @m@ on
--   which actions occur. For example, if your actions occur on
--   @'Control.Monad.IO.Class.MonadIO' m@ the parameter @c@ would be
--   'Control.Monad.IO.Class.MonadIO'. You can use 'EmptyConst' if
--   your context is has no constraints.
--
-- * The type parameter @m@ is the context on which actions occur.
--
-- * The type parameter @r@ is the return value of actions.
--
-- These parameters determine the type of event handlers. For example,
--
-- * @'EventDispatcher' 'Control.Monad.IO.Class.MonadIO' m ()@ expects
--   event handlers of type @'Control.Monad.IO.Class.MonadIO' m ⇒ e →
--   m ()@ for some 'Event' @e@.
--
-- * @'EventDispatcher' 'EmptyConst' 'IO' ()@ expects event handlers
--   of type @e → 'IO' ()@ for some 'Event' @e@.
data EventDispatcher (c ∷ (* → *) → Constraint) (m ∷ * → *) r
  = EventDispatcher
    { _handlers ∷ !(TypeRepMap (EH c m r))
    , _fallback ∷ !(∀e. EventHandler e c m r)
    }
makeLenses ''EventDispatcher

-- | A helper constraint constructor that produces an empty
-- 'Constraint'.
class EmptyConst (m ∷ * → *)
instance EmptyConst m

-- | @'dispatcher' fb@ creates an empty dispatcher with a fallback
-- handler @fb@ which is a rank-2 polymorphic function taking any type
-- of 'Event' and runs an action.
dispatcher ∷ (∀e. (Event e, c m) ⇒ EventHandler e c m r ) → EventDispatcher c m r
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
-- 'addHandler' handleWSC ed -- where ed ∷ 'EventDispatcher' 'EmptyConst' 'IO' ()
-- @
--
-- The type of the handler function determines the type of 'Event' it
-- handles. There can be at most one handler for each event type. If
-- there is already a handler for the same event type, the old one is
-- discarded.
addHandler ∷ (Event e, c m) ⇒ EventHandler e c m r → EventDispatcher c m r → EventDispatcher c m r
addHandler h ed
  = ed & handlers %~ TRM.insert (EH h)

-- | @'dispatch' ed e@ invokes an event handler registered for the
-- event type of @e@ if there is one. Otherwise it invokes the
-- fallback handler.
dispatch ∷ c m ⇒ EventDispatcher c m r → SomeEvent → m r
dispatch ed (SomeEvent e)
  = case TRM.lookup (ed^.handlers) of
      Just (EH h) → h e
      Nothing     → (ed^.fallback) e
