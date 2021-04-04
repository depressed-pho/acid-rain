{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Entities
  ( Player(..)

  , loadEntities
  ) where

import Control.Eff (Eff, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Game.AcidRain.TUI
  ( HasAppearance(..), begin, end, (⊳), unicode, ascii, bold )
import Game.AcidRain.Module.Loader (LoaderContext, registerEntityType)
import Game.AcidRain.World.Command (modifyPlayer, fireEvent)
import Game.AcidRain.World.Entity (EntityType(..), Entity(..))
import Game.AcidRain.World.Player (PlayerID, PlayerMoved(..), plPos)
import Lens.Micro ((.~))


instance EntityType (Proxy Player) where
  type EntityOf (Proxy Player) = Player
  entityTypeID _ = "acid-rain:player"

data Player = Player !PlayerID deriving Show
instance Entity Player where
  type EntityTypeOf Player = Proxy Player
  entityType _ = Proxy
  entityMoved (Player pid) src dest
    = do let f = plPos .~ dest
         modifyPlayer f pid
         fireEvent $ PlayerMoved pid src dest

instance HasAppearance Player where
  appearance _
    = begin ⊳ unicode "@" ⊳ ascii '@' ⊳ bold ⊳ end


loadEntities ∷ (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadEntities
  = traverse_ registerEntityType
    [ upcastEntityType (Proxy ∷ Proxy Player)
    ]
