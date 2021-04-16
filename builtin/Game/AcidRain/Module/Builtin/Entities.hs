{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Entities
  ( loadEntities
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Game.AcidRain.TUI
  ( HasAppearance(..), begin, end, (⊳), unicode, ascii, bold )
import Game.AcidRain.Module.Loader (LoaderContext, registerEntityType)
import Game.AcidRain.World (modifyPlayer, fireEvent)
import Game.AcidRain.World.Entity (Entity(..), HasEntityType(..), SomeEntityType)
import Game.AcidRain.World.Player (PlayerID, PlayerMoved(..), plPos)
import Lens.Micro ((.~))


data Player = Player !PlayerID deriving Show
instance Entity Player where
  type EntityGeneOf Player = PlayerID
  entityTypeID _ = "acid-rain:player"
  instantiate _ pid = Player pid
  entityMoved (Player pid) src dest
    = do let f = plPos .~ dest
         modifyPlayer f pid
         fireEvent $ PlayerMoved pid src dest
instance HasAppearance Player where
  appearance _
    = begin ⊳ unicode "@" ⊳ ascii '@' ⊳ bold ⊳ end


loadEntities ∷ ∀r. (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadEntities
  = traverse_ go
    [ upcastEntityType (Proxy ∷ Proxy Player)
    ]
  where
    go ∷ SomeEntityType → Eff r ()
    go someET = withEntityType someET registerEntityType
