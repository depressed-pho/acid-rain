{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Entity.Registry
  ( -- * The registry type
    EntityRegistry

    -- * Constructing registries
  , empty

    -- * Manipulating registries
  , register

    -- * Querying registries
  , lookup
  , get

    -- * Exceptions
  , ConflictingEntityTypeIDException(..)
  , UnknownEntityTypeIDException(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.MonoTraversable
  ( Element, MonoFunctor, MonoFoldable, MonoTraversable, GrowingAppend
  , otraverse )
import Data.Proxy (Proxy)
import Game.AcidRain.World.Entity ( Entity(..), SomeEntityType, EntityTypeID )
import Prelude hiding (lookup)

-- | The entity registry is a data structure that contains types in
-- the class 'EntityType'. It is constructed while loading a world,
-- and becomes immutable afterwards.
newtype EntityRegistry = EntityRegistry (HashMap EntityTypeID SomeEntityType)
  deriving ( Show, MonoFunctor, MonoFoldable, GrowingAppend, Semigroup
           , Monoid )

-- GeneralisedNewtypeDeriving can't derive MonoTraversable for us due
-- to a limitation of the compiler. So we do it manually.
instance MonoTraversable EntityRegistry where
  otraverse f (EntityRegistry reg)
    = EntityRegistry <$> traverse f reg

type instance Element EntityRegistry = SomeEntityType

-- | Create an empty registry
empty ∷ EntityRegistry
empty = EntityRegistry HM.empty

-- | Register an entity type to the registry. Throws if it's already been
-- registered.
register ∷ (Entity ε, MonadThrow μ) ⇒ Proxy ε → EntityRegistry → μ EntityRegistry
register et (EntityRegistry reg)
  = let etid  = entityTypeID et
        someT = upcastEntityType et
    in
      case HM.member etid reg of
        True  → throwM $ ConflictingEntityTypeIDException etid
        False → return $ EntityRegistry $ HM.insert etid someT reg

-- | Lookup an entity type by its ID.
lookup ∷ EntityTypeID → EntityRegistry → Maybe (SomeEntityType)
lookup etid (EntityRegistry reg)
  = HM.lookup etid reg

-- | Get an entity type by its ID. Throws if it doesn't exist.
get ∷ MonadThrow μ ⇒ EntityTypeID → EntityRegistry → μ (SomeEntityType)
get etid reg
  = case lookup etid reg of
      Just et → return et
      Nothing → throwM $ UnknownEntityTypeIDException etid

-- | An exception to be thrown when two entity types with the same ID
-- is being registered.
data ConflictingEntityTypeIDException = ConflictingEntityTypeIDException !EntityTypeID
  deriving Show

instance Exception ConflictingEntityTypeIDException

-- | An exception to be thrown when there was no entity type having
-- the given ID.
data UnknownEntityTypeIDException = UnknownEntityTypeIDException !EntityTypeID
  deriving Show

instance Exception UnknownEntityTypeIDException
