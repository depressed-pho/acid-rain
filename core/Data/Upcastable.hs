{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax #-}
-- Turned out this wasn't useful at all, which is probably why there
-- wasn't equivalent type classes in Hackage. When you define a type
-- class with an associated wrapper, you still have to have a method
-- to wrap values in the class because otherwise you end up wrapping
-- already wrapped values. This module is currently unused, and is not
-- exposed even.

-- | This module provides a type class for upcastable classes, i.e.
-- classes having an associated type-erasing data type to wrap a
-- "polymorphic" data type into a "monomorphic" one. This is a
-- generalisation of the pattern found in 'Exception' whose
-- monomorphic counterpart is 'SomeException'. The same pattern is
-- almost universally found in OOP systems, especially GUI toolkits.
--
-- I was very surprised when I couldn't find a library in Hackage to
-- do this. So I decided to write one.
module Data.Upcastable
  ( Upcastable(..)
  , Downcastable(..)
  ) where

import Control.Exception.Base (Exception, SomeException, toException, fromException)
import Data.Dynamic (Dynamic, Typeable, toDyn, fromDyn, fromDynamic)
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)


-- | The type relation @'Upcastable' c w@ denotes that:
--
-- * An instance of @c@ can be upcasted to @w@, and
--
-- * @w@ can be downcasted back to @'Maybe' a@ such that @a@ is an
--   instance of @c@, and
--
-- * @w@ is itself an instance of @c@.
--
-- For example,
--
-- * An instance of 'Exception' can be upcasted to 'SomeException', and
--
-- * 'SomeException' can be downcasted back to @'Maybe' a@ such that
--   @a@ is an instance of 'Exception', and
--
-- * 'SomeException' is itself an instance of 'Exception'.
--
-- Also note that there can be many ways to erase the type of given @c
-- a@. For example a type @a@ which is an instance of 'Exception' can
-- be wrapped in 'SomeException' but it can also be wrapped in
-- 'Dynamic' if it's 'Typeable' too. This is why there is no
-- functional dependency from @c@ to @w@, nor @c@ has @w@ as an
-- associated type.
class (Downcastable w c, c w) ⇒ Upcastable (c ∷ * → Constraint) w where
  -- | The function to erase the type of @a@. For 'Exception' this is
  -- 'toException'.
  upcast ∷ c a ⇒ a → w

-- | The type relation @'Downcastable' w c@ defines how to downcast a
-- wrapped type @w@ to its unwrapped form. Since downcasting doesn't
-- always succeed (i.e. the actual types must match), the method
-- 'downcast' returns 'Maybe'. Note that the type @c@ is uniquely
-- determined by the type @w@.
class Downcastable w (c ∷ * → Constraint) | w → c where
  -- | The function to unwrap @w@. For 'SomeException' this is
  -- 'fromException'.
  downcast ∷ c a ⇒ w → Maybe a

  -- | A variant of 'downcast' taking a default value to fallback when
  -- types don't match.
  downcast' ∷ c a ⇒ w → a → a
  downcast' w d = fromMaybe d (downcast w)

instance Upcastable Exception SomeException where
  upcast = toException

instance Downcastable SomeException Exception where
  downcast = fromException


-- | Any 'Typeable' types can be upcasted to 'Dynamic' using
-- 'toDyn'.
instance Upcastable Typeable Dynamic where
  upcast = toDyn

-- | 'Dynamic' can be upcasted to any @a@ using 'fromDynamic' as long
-- as @a@ is 'Typeable'.
instance Downcastable Dynamic Typeable where
  downcast = fromDynamic
  downcast' = fromDyn
