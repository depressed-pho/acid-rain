{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Tile.Registry
  ( TileRegistry
  , empty
  , register
  , lookup
  , ConflictingTileIDException
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Game.AcidRain.World.Tile (Tile(..), SomeTile(..))
import Prelude hiding (lookup)


-- | The tile registry is struct that contains immutable Tile
-- objects. It is constructed while loading a world, and becomes
-- immutable afterwards.
newtype TileRegistry = TileRegistry (HashMap Text SomeTile)

-- | Create an empty registry.
empty ∷ TileRegistry
empty = TileRegistry HM.empty

-- | Register a tile to the registry.
register ∷ (MonadThrow m, Tile τ) ⇒ TileRegistry → τ → m TileRegistry
register (TileRegistry reg) tile =
  let tid = tileID tile
  in
    case HM.member tid reg of
      True  → throwM ConflictingTileIDException
      False → return $ TileRegistry $ HM.insert tid (SomeTile tile) reg

-- | Lookup a tile by its ID.
lookup ∷ Text → TileRegistry → Maybe SomeTile
lookup tid (TileRegistry reg) =
  HM.lookup tid reg

-- | An exception to be thrown when two tiles with the same ID is
-- being registered.
data ConflictingTileIDException = ConflictingTileIDException
  deriving Show

instance Exception ConflictingTileIDException
