{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Chunk
  ( TileOffset(..)
  , Chunk
  , chunkSize
  , chunkHeight

    -- * Constructing chunks
  , new
  , putEntity
  , deleteEntity

    -- * Querying chunks
  , tileStateAt
  , entityAt
  ) where

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow)
import Data.Default (Default(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Generic as GV
import Game.AcidRain.World.Chunk.Palette (TilePalette, indexOf, idOf)
import Game.AcidRain.World.Chunk.Types
  ( Chunk(..), TileOffset(..), IndexedTileState(..)
  , cTileReg, cTilePal, cTiles, cEntCat, cEntities, chunkSize, chunkHeight )
import Game.AcidRain.World.Entity (EntityType(..), Entity(..), SomeEntity)
import Game.AcidRain.World.Entity.Catalogue (EntityCatalogue, (∈))
import Game.AcidRain.World.Tile (Tile(..), TileState(..), SomeTileState)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Lens.Micro ((&), (^.), (%~))
import Prelude.Unicode ((∘), (⋅))


toIndexed ∷ MonadThrow μ ⇒ TilePalette → TileState τ → μ IndexedTileState
toIndexed palette (TileState { tsTile, tsValue })
  = do idx ← indexOf (tileID tsTile) palette
       return $ IndexedTileState
         { itsIndex = idx
         , itsValue = tsValue
         }

-- Assert that the given offset is valid.
assertValidOffset ∷ TileOffset → α → α
assertValidOffset (TileOffset { x, y, z })
  = assert (x < chunkSize) ∘
    assert (y < chunkSize) ∘
    assert (z < chunkHeight)

-- Assert that the given entity is in the catalogue.
assertValidEntity ∷ Entity ε ⇒ ε → Chunk → α → α
assertValidEntity e c
  = assert (entityTypeID (entityType e) ∈ c^.cEntCat)

-- | Create a chunk filled with a single specific tile which is
-- usually @acid-rain:air@.
new ∷ MonadThrow μ
    ⇒ TileRegistry
    → TilePalette
    → EntityCatalogue
    → TileState τ
    → μ Chunk
new tReg tPal eCat fill
  = do its ← toIndexed tPal fill
       return $ Chunk
         { _cTileReg  = tReg
         , _cTilePal  = tPal
         , _cTiles    = GV.replicate (chunkSize ⋅ chunkSize ⋅ chunkHeight) its
         , _cClimates = GV.replicate (chunkSize ⋅ chunkSize) def
         , _cEntCat   = eCat
         , _cEntities = HM.empty
         }

-- | Place an entity at a given offset in a chunk.
putEntity ∷ Entity ε ⇒ TileOffset → ε → Chunk → Chunk
putEntity off e c
  = assertValidOffset off $
    assertValidEntity e c $
    c & cEntities %~ HM.insert off (upcastEntity e)

-- | Remove an entity at a given offset in a chunk if any.
deleteEntity ∷ TileOffset → Chunk → Chunk
deleteEntity off c
  = assertValidOffset off $
    c & cEntities %~ HM.delete off

-- | Get a tile state at a given offset in a chunk.
tileStateAt ∷ MonadThrow μ ⇒ TileOffset → Chunk → μ SomeTileState
tileStateAt off@(TileOffset { x, y, z }) c
  = assertValidOffset off $
    let x'  = fromIntegral x ∷ Int
        y'  = fromIntegral y ∷ Int
        z'  = fromIntegral z ∷ Int
        its = (c^.cTiles) GV.! (z' ⋅ chunkHeight ⋅ chunkSize + y' ⋅ chunkSize + x')
    in
      do tid  ← idOf (itsIndex its) (c^.cTilePal)
         tile ← TR.get tid (c^.cTileReg)
         return $ TileState
           { tsTile  = tile
           , tsValue = itsValue its
           }

-- | Lookup an entity possibly located at a given offset in a chunk.
entityAt ∷ TileOffset → Chunk → Maybe SomeEntity
entityAt off c
  = assertValidOffset off $
    HM.lookup off (c^.cEntities)
