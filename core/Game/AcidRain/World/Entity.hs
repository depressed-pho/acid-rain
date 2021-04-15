{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Entity
  ( -- * Entity types
    EntityType(..)
  , EntityTypeID

    -- * Entity instances
  , Entity(..)
  ) where

import Control.Eff (Eff, Lifted, type(<::))
import Control.Eff.Exception (Exc)
import Control.Eff.State.Strict (State)
import Control.Exception (SomeException)
import Control.Monad.STM (STM)
import Data.Kind (Type)
import Data.Poly.Strict (Poly(..))
import Data.Text (Text)
import Game.AcidRain.TUI (HasAppearance(..))
import {-# SOURCE #-} Game.AcidRain.World (WorldCtx)
import Game.AcidRain.World.Position (WorldPos)


type EntityTypeID = Text

-- | An instance of this class defines an entity type in the game. The
-- purpose of entity types is to define how individual entities are
-- spawned, serialised, or deserialised.
--
-- Unlike tiles, every entity in a chunk is individually serialized on
-- disk and instantiated in memory. But worlds still have a catalogue
-- of all the possible entity IDs because otherwise we won't know what
-- to do with entities from modules that no longer exist.
--
-- There are several entity types that have a special role in the
-- game:
--
-- * @acid-rain:player@ represents a player and merely has a player
--   ID. The actual data about the player is separately stored in the
--   player database.
--
-- There can be no more than one entity at the same world position
-- (Pauli exclusion principle). Item piles are /not/ entities.
--
-- Note that an instance of 'EntityType' typically has no values, and
-- instead implements the class on 'Data.Proxy.Proxy' like:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- data Player
-- instance 'EntityType' ('Data.Proxy.Proxy' Player) where
--   entityID _ = "acid-rain:player"
--   ..
-- @
class Show τ ⇒ EntityType τ where
  -- | The entity instance of this entity type. It has to be in the
  -- class 'Entity'.
  type EntityOf τ ∷ Type
  -- | Erase the type of the entity type.
  upcastEntityType ∷ τ → Poly EntityType
  upcastEntityType = Poly
  -- | Get the entity type ID such as @acid-rain:player@.
  entityTypeID ∷ τ → EntityTypeID

instance Show (Poly EntityType) where
  showsPrec d (Poly t) = showsPrec d t

instance EntityType (Poly EntityType) where
  type EntityOf (Poly EntityType) = Poly Entity
  upcastEntityType = id
  entityTypeID (Poly t) = entityTypeID t

-- | An instance of this class defines an entity in the game. It is
-- instantiated through an associated type 'EntityT'.
class (EntityType (EntityTypeOf ε), HasAppearance ε, Show ε) ⇒ Entity ε where
  -- | The entity type of this entity. It has to be in the class
  -- 'EntityType'.
  type EntityTypeOf ε ∷ Type
  -- | Erase the type of the entity.
  upcastEntity ∷ ε → Poly Entity
  upcastEntity = Poly
  -- | Get the type of this entity.
  entityType ∷ ε → EntityTypeOf ε
  -- | Called when an entity has been moved. Do nothing by default.
  entityMoved ∷ (Lifted STM r, [State WorldCtx, Exc SomeException] <:: r)
              ⇒ ε
              → WorldPos -- ^ from
              → WorldPos -- ^ to
              → Eff r ()
  entityMoved _ _ _ = return ()

instance Show (Poly Entity) where
  showsPrec d (Poly e) = showsPrec d e

instance HasAppearance (Poly Entity) where
  appearance (Poly e) = appearance e

instance Entity (Poly Entity) where
  type EntityTypeOf (Poly Entity) = Poly EntityType
  upcastEntity = id
  entityType (Poly e) = Poly (entityType e)
  entityMoved (Poly e) = entityMoved e
