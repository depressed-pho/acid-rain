{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}
module Data.Poly.Strict
  ( Poly(..)
  ) where

import Data.Kind (Type, Constraint)


-- | A variant of @Poly@ from
-- <http://hackage.haskell.org/package/polydata-core
-- polydata-core>. The term constructor 'Poly' is strict in the
-- argument.
data Poly (c ∷ Type → Constraint) where
  Poly ∷ c a ⇒ !a → Poly c
