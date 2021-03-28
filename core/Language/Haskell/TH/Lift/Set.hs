{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Language.Haskell.TH.Lift.Set
  ( liftSet
  , genericLiftSet
  ) where

import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic, Rep)
import Language.Haskell.TH (Q, Exp(ListE))
import Language.Haskell.TH.Lift.Generics (GLift, genericLift)
import Language.Haskell.TH.Syntax (Lift(..))


liftSet ∷ Lift α ⇒ Set α → Q Exp
liftSet s
  = let xs = S.toList s
    in [| S.fromList xs |]

genericLiftSet ∷ (Generic α, GLift (Rep α)) ⇒ Set α → Q Exp
genericLiftSet s
  = do xs ← traverse genericLift $ S.toList s
       [| S.fromList $(return $ ListE xs) |]
