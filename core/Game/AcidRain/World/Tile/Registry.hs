{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile.Registry
  ( TileRegistry
  , empty
  , register
  , find
  , ConflictingTileIDException(..)
  , UnknownTileIDException(..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Game.AcidRain.World.Tile (Tile(..), TileID, SomeTile(..))


-- | The tile registry is struct that contains immutable Tile
-- objects. It is constructed while loading a world, and becomes
-- immutable afterwards.
newtype TileRegistry = TileRegistry (HashMap TileID SomeTile)

-- | Create an empty registry.
empty ∷ TileRegistry
empty = TileRegistry HM.empty

-- | Register a tile to the registry.
register ∷ (MonadThrow μ, Tile τ) ⇒ TileRegistry → τ → μ TileRegistry
register (TileRegistry reg) tile
  = let tid = tileID tile
    in
      case HM.member tid reg of
        True  → throwM $ ConflictingTileIDException tid
        False → return $ TileRegistry $ HM.insert tid (SomeTile tile) reg

-- | Lookup a tile by its ID.
find ∷ MonadThrow μ ⇒ TileID → TileRegistry → μ SomeTile
find tid (TileRegistry reg)
  = case HM.lookup tid reg of
      Just tile → return tile
      Nothing   → throwM $ UnknownTileIDException tid

-- | An exception to be thrown when two tiles with the same ID is
-- being registered.
data ConflictingTileIDException = ConflictingTileIDException TileID
  deriving Show

instance Exception ConflictingTileIDException

-- | An exception to be thrown when there was no tile having the given
-- ID.
data UnknownTileIDException = UnknownTileIDException TileID
  deriving Show

instance Exception UnknownTileIDException
