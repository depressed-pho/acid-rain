{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile.Registry
  ( -- * The registry type
    TileRegistry

    -- * Constructing registries
  , empty

    -- * Manipulating registries
  , register

    -- * Querying registries
  , lookup
  , get

    -- * Exceptions
  , ConflictingTileIDException(..)
  , UnknownTileIDException(..)
  ) where

import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.MonoTraversable
  ( Element, MonoFunctor, MonoFoldable, MonoTraversable, GrowingAppend
  , otraverse )
import Game.AcidRain.World.Tile (Tile(..), TileID, SomeTile(..))
import Prelude hiding (lookup)

-- | The tile registry is a data structure that contains immutable
-- 'Tile' objects. It is constructed while loading a world, and
-- becomes immutable afterwards.
newtype TileRegistry = TileRegistry (HashMap TileID SomeTile)
  deriving ( Show, MonoFunctor, MonoFoldable, GrowingAppend, Semigroup
           , Monoid )

-- GeneralisedNewtypeDeriving can't derive MonoTraversable for us due
-- to a limitation of the compiler. So we do it manually.
instance MonoTraversable TileRegistry where
  otraverse f (TileRegistry reg)
    = TileRegistry <$> traverse f reg

type instance Element TileRegistry = SomeTile

-- | Create an empty registry.
empty ∷ TileRegistry
empty = TileRegistry HM.empty

-- | Register a tile to the registry. Throws if it's already been
-- registered.
register ∷ (Tile τ, MonadThrow μ) ⇒ τ → TileRegistry → μ TileRegistry
register tile (TileRegistry reg)
  = let tid = tileID tile
    in
      case HM.member tid reg of
        True  → throwM $ ConflictingTileIDException tid
        False → return $ TileRegistry $ HM.insert tid (upcastTile tile) reg

-- | Lookup a tile by its ID.
lookup ∷ TileID → TileRegistry → Maybe SomeTile
lookup tid (TileRegistry reg)
  = HM.lookup tid reg

-- | Get a tile by its ID. Throws if it doesn't exist.
get ∷ MonadThrow μ ⇒ TileID → TileRegistry → μ SomeTile
get tid reg
  = case lookup tid reg of
      Just tile → return tile
      Nothing   → throwM $ UnknownTileIDException tid

-- | An exception to be thrown when two tiles with the same ID is
-- being registered.
data ConflictingTileIDException = ConflictingTileIDException !TileID
  deriving Show

instance Exception ConflictingTileIDException

-- | An exception to be thrown when there was no tile having the given
-- ID.
data UnknownTileIDException = UnknownTileIDException !TileID
  deriving Show

instance Exception UnknownTileIDException
