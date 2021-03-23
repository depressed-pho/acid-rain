{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Biomes
  ( loadBiomes
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Game.AcidRain.Module.Loader (LoaderContext, registerBiome)


loadBiomes ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadBiomes
  = return ()
{-
  = traverse_ registerBiome
    [ upcastTile (Proxy ∷ Proxy Air)
    ]
-}
