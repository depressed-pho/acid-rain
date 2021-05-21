{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Language.Haskell.TH.Lift.Set
  ( liftSet
  , liftSetTyped
  , genericLiftSet
  , genericLiftSetTyped
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic, Rep)
import Language.Haskell.TH.Lift.Generics (GLift, genericLift)
import Language.Haskell.TH.Syntax
  ( Quote, Lift(..), Code(..), Exp(ListE), unTypeCode, unsafeTExpCoerce )
import Prelude.Unicode ((∘))


liftSet ∷ (Quote μ, Lift α, Ord α) ⇒ Set α → μ Exp
liftSet = unTypeCode ∘ liftSetTyped

liftSetTyped ∷ (Quote μ, Lift α, Ord α) ⇒ Set α → Code μ (Set α)
liftSetTyped s
  = let xs = S.toList s
    in [|| S.fromList xs ||]

genericLiftSet ∷ (Quote μ, Generic α, GLift (Rep α)) ⇒ Set α → μ Exp
genericLiftSet = unTypeCode ∘ genericLiftSetTyped

genericLiftSetTyped ∷ (Quote μ, Generic α, GLift (Rep α)) ⇒ Set α → Code μ (Set α)
genericLiftSetTyped s
  = Code $
    do xs ← traverse genericLift $ S.toList s
       -- I don't like this coercion, but how can we do this without
       -- it? We can create a list of @Code μ α@ via genericLiftTyped
       -- but I can't find a way to splice it.
       unsafeTExpCoerce [| S.fromList $(return (ListE xs)) |]
