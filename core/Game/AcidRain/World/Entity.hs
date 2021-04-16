{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Entity
  ( Entity(..)
  , EntityTypeID

  , HasEntity(..)

  , HasEntityType(..)
  , SomeEntityType(..)
  , dynInstantiate
  ) where

import Control.Eff (Eff, Lifted, type(<::))
import Control.Eff.Exception (Exc)
import Control.Eff.State.Strict (State)
import Control.Exception (SomeException)
import Control.Monad.STM (STM)
import Data.Kind (Type)
import Data.Poly.Strict (Poly(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Type.Equality (TestEquality(..))
import GHC.Stack (HasCallStack)
import Game.AcidRain.TUI (HasAppearance(..))
import {-# SOURCE #-} Game.AcidRain.World (WorldCtx)
import Game.AcidRain.World.Position (WorldPos)
import Type.Reflection (Typeable, (:~:)(Refl), typeOf, typeRep)


type EntityTypeID = Text

-- | An instance of this class defines an entity in the game.
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
class (HasAppearance ε, Show ε, Typeable (EntityGeneOf ε)) ⇒ Entity ε where
  -- | Data needed for instantiating this entity.
  type EntityGeneOf ε ∷ Type
  -- | Erase the type of the entity type.
  upcastEntityType ∷ Proxy ε → SomeEntityType
  upcastEntityType = SomeEntityType
    -- | Erase the type of the entity.
  upcastEntity ∷ ε → Poly Entity
  upcastEntity = Poly
  -- | Get the entity type ID such as @acid-rain:player@.
  entityTypeID ∷ Proxy ε → EntityTypeID
  -- | Create an instance of this entity.
  instantiate ∷ Proxy ε → EntityGeneOf ε → ε
  -- | Called when an entity has been moved. Do nothing by default.
  entityMoved ∷ (Lifted STM r, [State WorldCtx, Exc SomeException] <:: r)
              ⇒ ε
              → WorldPos -- ^ from
              → WorldPos -- ^ to
              → Eff r ()
  entityMoved _ _ _ = return ()

-- | Type-erased proxy of an 'Entity'.
data SomeEntityType = ∀ε. Entity ε ⇒ SomeEntityType !(Proxy ε)

instance Show SomeEntityType where
  showsPrec d (SomeEntityType t) = showsPrec d t

instance Show (Poly Entity) where
  showsPrec d (Poly e) = showsPrec d e

instance HasAppearance (Poly Entity) where
  appearance (Poly e) = appearance e

class HasEntityType τ where
  -- | Evaluate a function with a proxy of some 'Entity'.
  withEntityType ∷ τ → (∀ε. Entity ε ⇒ Proxy ε → α) → α

-- | Attempt to create an instance of entity using an any type as a
-- gene. If the runtime type of genes don't match, this throws an
-- exception.
dynInstantiate ∷ (HasCallStack, HasEntityType τ, Typeable g) ⇒ τ → g → Poly Entity
dynInstantiate t g
  = withEntityType t $ \(proxy ∷ Proxy ε) →
    -- This works, but I have no idea why. The type of ε is available
    -- at run time, but how can GHC retrieve the type of (EntityGeneOf
    -- ε)? It's an associated type family after all. Is it perhaps a
    -- part of the dictionary of Entity?
    let expectedType = typeRep @(EntityGeneOf ε)
        gotType      = typeOf g
    in
      case testEquality expectedType gotType of
        Just Refl → Poly $ instantiate proxy g
        Nothing   → error ("Entity gene types don't match: expected " ++
                           show expectedType ++ " but got " ++ show gotType)

instance HasEntityType (Poly Entity) where
  withEntityType (Poly (_ ∷ ε)) f
    = f (Proxy ∷ Proxy ε)

instance HasEntityType SomeEntityType where
  withEntityType (SomeEntityType proxy) f = f proxy

class HasEntity τ where
  -- | Evaluate a function with some 'Entity'.
  withEntity ∷ τ → (∀ε. Entity ε ⇒ ε → α) → α

instance HasEntity (Poly Entity) where
  withEntity (Poly e) f = f e
