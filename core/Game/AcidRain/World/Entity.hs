{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Entity
  ( -- * Entity types
    EntityType(..)
  , EntityTypeID
  , SomeEntityType(..)

    -- * Entity instances
  , Entity(..)
  , SomeEntity
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Game.AcidRain.TUI (HasAppearance(..))


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
-- data PlayerT
-- instance 'EntityType' ('Data.Proxy.Proxy' PlayerT) where
--   entityID _ = "acid-rain:player"
--   ..
-- @
class (τ ~ EntityTypeOf (EntityOf τ), Entity (EntityOf τ), Show τ) ⇒ EntityType τ where
  -- | The entity instance of this entity type. It has to be in the
  -- class 'Entity'.
  type EntityOf τ ∷ Type
  -- | Erase the type of the entity type.
  upcastEntityType ∷ τ → SomeEntityType
  upcastEntityType = SomeEntityType
  -- | Get the entity type ID such as @acid-rain:player@.
  entityTypeID ∷ τ → EntityTypeID
  -- | Spawn an entity of this type.
  spawnEntity ∷ τ → EntityOf τ -- FIXME: more data

-- | A type-erased 'EntityType'.
data SomeEntityType = ∀τ. EntityType τ ⇒ SomeEntityType !τ

instance Show SomeEntityType where
  showsPrec d (SomeEntityType t) = showsPrec d t

instance EntityType SomeEntityType where
  type EntityOf SomeEntityType = SomeEntity
  upcastEntityType = id
  entityTypeID (SomeEntityType t) = entityTypeID t
  spawnEntity (SomeEntityType t) = SomeEntity (spawnEntity t)

-- | An instance of this class defines an entity in the game. It is
-- instantiated through an associated type 'EntityT'.
class HasAppearance ε ⇒ Entity ε where
  -- | The entity type of this entity. It has to be in the class
  -- 'EntityType'.
  type EntityTypeOf ε ∷ Type
  -- | Erase the type of the entity.
  upcastEntity ∷ EntityType (EntityTypeOf ε) ⇒ ε → SomeEntity
  upcastEntity = SomeEntity
  -- | Get the type of this entity.
  entityType ∷ EntityType (EntityTypeOf ε) ⇒ ε → EntityTypeOf ε

-- | A type-erased 'Entity'.
data SomeEntity = ∀ε. (EntityType (EntityTypeOf ε), Entity ε) ⇒ SomeEntity !ε

instance HasAppearance SomeEntity where
  appearance (SomeEntity e) = appearance e

instance Entity SomeEntity where
  type EntityTypeOf SomeEntity = SomeEntityType
  upcastEntity = id
  entityType (SomeEntity e) = SomeEntityType (entityType e)
