{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Biomes
  ( -- * Per-biome chunk generator
    BiomeChunkGen(..)

    -- * Special biomes that are chosen independently of climate.

    -- * Biomes that are determined base on 'Climate'
  , BiomeSet
  , climateBasedBiomes

    -- * Registering biomes to the core
  , loadBiomes
  ) where

import Control.Eff (Eff, Lifted, Member)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy(..))
import Game.AcidRain.World.Biome (Biome(..))
import Game.AcidRain.World.Chunk.Generator (ChunkGenM)
import Game.AcidRain.World.Climate (Climate(..))
import Game.AcidRain.World.Position (WorldPos)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import Game.AcidRain.Module.Loader (LoaderContext, registerBiome)


-- | A type in this class is a 'Biome' which also knows how to
-- terraform and decorate chunks.
class (Biome (Proxy β)) ⇒ BiomeChunkGen β where
  -- | Constructor in OOP sense.
  instantiate ∷ MonadThrow μ ⇒ Proxy β → TileRegistry → μ β
  -- | Representative climate of this biome.
  climate ∷ Proxy β → Climate
  -- | Place tiles at the given @(x, y)@ coordinates for all @z@
  -- positions.
  terraform ∷ Lifted ChunkGenM r ⇒ β → Float → WorldPos → Eff r ()

data SomeBiomeProxy = ∀β. BiomeChunkGen β ⇒ SomeBiomeProxy !(Proxy β)
data SomeBiomeGen   = ∀β. BiomeChunkGen β ⇒ SomeBiomeGen !β

type BiomeSet = HashMap Climate SomeBiomeGen


data Plains = Plains
instance Biome (Proxy Plains) where
  biomeID _ = "acid-rain:plains"
instance BiomeChunkGen Plains where
  instantiate _ _ = return Plains
  climate _ = Climate
              { cliTemperature = 18
              , cliHumidity    = 0.5
              , cliAltitude    = 400
              }
  terraform _ _height _wPos0
    = error "FIXME"


-- | A map from representative climate to its corresponding biome
-- proxy. Some biomes like 'River' and 'Ocean' are not chosen based on
-- climate, so they aren't listed in this map. Ideally we should be
-- creating an n-dimensional spatial index, but for now we don't do
-- it. <http://hackage.haskell.org/package/pktree pktree> is quite
-- promising.
climateBasedBiomeProxies ∷ HashMap Climate SomeBiomeProxy
{-# NOINLINE climateBasedBiomeProxies #-}
climateBasedBiomeProxies
  = HM.fromList $ map toPair $
    [ SomeBiomeProxy (Proxy ∷ Proxy Plains)
    ]
  where
    toPair sbp@(SomeBiomeProxy bp) = (climate bp, sbp)

-- | Instantiate climate-based biomes.
climateBasedBiomes ∷ MonadThrow μ ⇒ TileRegistry → μ BiomeSet
climateBasedBiomes tReg
  = flip traverse climateBasedBiomeProxies $
    \(SomeBiomeProxy bp) →
      SomeBiomeGen <$> instantiate bp tReg

loadBiomes ∷ ∀r. (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadBiomes
  = do traverse_ register' climateBasedBiomeProxies
  where
    register' ∷ SomeBiomeProxy → Eff r ()
    register' (SomeBiomeProxy bp)
      = registerBiome bp
