{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Entity.Catalogue
  ( -- * The catalogue type
    EntityCatalogue

    -- * Constructing catalogues
  , empty
  , fromRegistry

    -- * Manipulating catalogues
  , insert
  ) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.MonoTraversable (ofoldl')
import Game.AcidRain.World.Entity (EntityType(..), EntityTypeID)
import Game.AcidRain.World.Entity.Registry (EntityRegistry)
import Lens.Micro ((&), (%~))
import Lens.Micro.TH (makeLenses)


-- | Entity catalogue is a set of entity type IDs that can possibly
-- exist in a world. Like palette index, entity catalogue is also
-- saved on disk.
newtype EntityCatalogue
  = EntityCatalogue
    { _etids ∷ HashSet EntityTypeID
    } deriving Show

makeLenses ''EntityCatalogue

-- | Create an empty catalogue.
empty ∷ EntityCatalogue
empty = EntityCatalogue HS.empty

-- | Construct a catalogue out of an entity registry by copying all
-- the entity type IDs from the registry. This is only useful while
-- creating a fresh new world, as catalogues must contain every
-- possible entity type ID that can appear in any existing chunks.
fromRegistry ∷ EntityRegistry → EntityCatalogue
fromRegistry = ofoldl' (\c et → insert (entityTypeID et) c) empty

-- | Insert an entity type ID in the catalogue. Inserting the same ID
-- twice is not an error. It will just be ignored.
insert ∷ EntityTypeID → EntityCatalogue → EntityCatalogue
insert eid cat
  = cat & etids %~ HS.insert eid
