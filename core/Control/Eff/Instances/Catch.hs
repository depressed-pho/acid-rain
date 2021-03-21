{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Orphan instance of 'MonadThrow' for the 'Eff' monad.
module Control.Eff.Instances.Catch () where

import Control.Eff (Eff, Lifted, lift)
import Control.Monad.Catch (MonadThrow(..))
import Prelude.Unicode ((∘))

instance (MonadThrow μ, Lifted μ r) ⇒ MonadThrow (Eff r) where
  throwM = lift ∘ throwM
