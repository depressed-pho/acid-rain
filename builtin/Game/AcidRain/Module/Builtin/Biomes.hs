{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.Module.Builtin.Biomes
  ( -- * Per-biome chunk generator
    BiomeChunkGen(..)
  , SomeBiomeGen
  , withBiomeGenProxy
  , withBiomeGen

    -- * Biome chooser
  , BiomeChooser
  , biomeChooser
  , chooseBiome

    -- * Registering biomes to the core
  , loadBiomes
  ) where

import Control.Eff (Eff, Lifted, Member)
import Control.Eff.Reader.Lazy (Reader)
import Control.Eff.State.Strict (State)
import Control.Monad.Catch (MonadThrow)
import Data.Convertible.Base (convert)
import Data.Foldable (traverse_, for_, foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int8)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeOf)
import Game.AcidRain.Module.Builtin.ChunkGen.WorldInfo (WorldInfo(..))
import Game.AcidRain.World.Biome (Biome(..), SomeBiome(..))
import Game.AcidRain.World.Chunk (chunkHeight)
import Game.AcidRain.World.Chunk.Generator (ChunkGenM, putTileState)
import Game.AcidRain.World.Climate (Climate(..))
import Game.AcidRain.World.Position (WorldPos, wpZ, lowestZ)
import Game.AcidRain.World.Tile (SomeTileState, defaultState)
import Game.AcidRain.World.Tile.Registry (TileRegistry)
import qualified Game.AcidRain.World.Tile.Registry as TR
import Game.AcidRain.Module.Loader (LoaderContext, registerBiome)
import Lens.Micro ((&), (.~))
import Prelude.Unicode ((⋅), (≤), (∧))


-- | A type in this class is a 'Biome' which also knows how to
-- terraform and decorate chunks.
class (Biome (Proxy β), Typeable β) ⇒ BiomeChunkGen β where
  -- | Constructor in OOP sense.
  instantiate ∷ MonadThrow μ ⇒ Proxy β → TileRegistry → μ β
  -- | Representative climate of this biome.
  climate ∷ Proxy β → Climate
  -- | The biome type corresponding to this generator.
  biomeType ∷ Proxy β → SomeBiome
  biomeType p = SomeBiome p
  -- | Place tiles at the given @(x, y)@ coordinates for all @z@
  -- positions.
  terraform ∷ (Member (Reader WorldInfo) r, Lifted ChunkGenM r)
            ⇒ β
            → Double -- ^ Height in @[-1, 1]@
            → WorldPos
            → Eff r ()

data SomeBiomeProxy = ∀β. BiomeChunkGen β ⇒ SomeBiomeProxy !(Proxy β)
data SomeBiomeGen   = ∀β. BiomeChunkGen β ⇒ SomeBiomeGen !β

instance Show SomeBiomeGen where
  showsPrec d (SomeBiomeGen b)
    = showsPrec d (typeOf b)

-- | Evaluate a function with a 'BiomeChunkGen'.
withBiomeGen ∷ SomeBiomeGen → (∀β. BiomeChunkGen β ⇒ β → α) → α
withBiomeGen (SomeBiomeGen b) f = f b

-- | Evaluate a function with a proxy of 'BiomeChunkGen'.
withBiomeGenProxy ∷ SomeBiomeGen → (∀β. BiomeChunkGen β ⇒ Proxy β → α) → α
withBiomeGenProxy (SomeBiomeGen (_ ∷ β)) f
  = f (Proxy ∷ Proxy β)

type BiomeSet = HashMap Climate SomeBiomeGen

data BiomeChooser
  = BiomeChooser
    { bcClimateBased ∷ !BiomeSet
    , bcOcean        ∷ !SomeBiomeGen
    , bcRiver        ∷ !SomeBiomeGen
    }

biomeChooser ∷ MonadThrow μ ⇒ TileRegistry → μ BiomeChooser
biomeChooser tReg
  = do climateBased ← climateBasedBiomes tReg
       ocean        ← instantiate (Proxy ∷ Proxy Ocean) tReg
       river        ← instantiate (Proxy ∷ Proxy River) tReg
       return BiomeChooser
         { bcClimateBased = climateBased
         , bcOcean        = SomeBiomeGen ocean
         , bcRiver        = SomeBiomeGen river
         }

chooseBiome ∷ Double → Double → Climate → BiomeChooser → SomeBiomeGen
chooseBiome height river cli bc
  | height < 0  = bcOcean bc
  | river > 0.7 = bcRiver bc
  | otherwise   = chooseBiomeForClimate cli (bcClimateBased bc)

-- | Get a remapped height in @[-1, 0]@. The sea level becomes @-1@
-- after remapping, and nearly every height is also remapped to
-- @-1@. Only a few become @0@.
remappedHeight ∷ Double → Int8
remappedHeight height0
  = -- The ground is below the seal level.
    if height0 ≤ 0 then
      lowestZ - 1
    else
      -- And now this is a hard question. How exactly should we remap
      -- heights above the sea level? For now we simply apply a cut
      -- off at a certain constant height.
      if height0 ≤ cutOff then
        lowestZ
      else
        lowestZ + 1
  where
    cutOff = 0.52 --0.78

-------------------------------------------------------------------------------
-- Individual biomes
-------------------------------------------------------------------------------
data Ocean = Ocean { sand     ∷ !SomeTileState
                   , seawater ∷ !SomeTileState }
instance Biome (Proxy Ocean) where
  biomeID _ = "acid-rain:ocean"
instance BiomeChunkGen Ocean where
  instantiate _ tReg
    = do sand     ← defaultState <$> TR.get "acid-rain:sand"     tReg
         seawater ← defaultState <$> TR.get "acid-rain:seawater" tReg
         return Ocean { sand, seawater }
  climate _ = error "This biome is not climate-based"
  terraform (Ocean { sand, seawater }) height wPos0
    = do let rHeight = remappedHeight height
         for_ [lowestZ .. lowestZ+chunkHeight-1] $ \z →
           do let wPos = wPos0 & wpZ .~ z
                  off  = convert wPos
              case z of
                _ | z < 0 ∧ rHeight < lowestZ → putTileState off seawater
                  | z ≤ rHeight               → putTileState off sand
                  | otherwise                 → return ()

data Plains = Plains { dirt  ∷ !SomeTileState
                     , water ∷ !SomeTileState }
instance Biome (Proxy Plains) where
  biomeID _ = "acid-rain:plains"
instance BiomeChunkGen Plains where
  instantiate _ tReg
    = do dirt  ← defaultState <$> TR.get "acid-rain:dirt"  tReg
         water ← defaultState <$> TR.get "acid-rain:water" tReg
         return Plains { dirt, water }
  climate _ = Climate
              { cliTemperature = 18
              , cliHumidity    = 0.5
              , cliAltitude    = 400
              }
  terraform (Plains { dirt, water }) height wPos0
    = do let rHeight = remappedHeight height
         for_ [lowestZ .. lowestZ+chunkHeight-1] $ \z →
           do let wPos = wPos0 & wpZ .~ z
                  off  = convert wPos
              case z of
                _ | z < 0 ∧ rHeight < lowestZ → putTileState off water
                  | z ≤ rHeight               → putTileState off dirt
                  | otherwise                 → return ()

data River = River { gravel ∷ !SomeTileState
                   , water  ∷ !SomeTileState }
instance Biome (Proxy River) where
  biomeID _ = "acid-rain:river"
instance BiomeChunkGen River where
  instantiate _ tReg
    = do gravel ← defaultState <$> TR.get "acid-rain:gravel" tReg
         water  ← defaultState <$> TR.get "acid-rain:water"  tReg
         return River { gravel, water }
  climate _ = error "This biome is not climate-based"
  terraform (River { gravel, water }) height wPos0
    = do let rHeight = remappedHeight height
         for_ [lowestZ .. lowestZ+chunkHeight-1] $ \z →
           do let wPos = wPos0 & wpZ .~ z
                  off  = convert wPos
              case z of
                _ | z < 0 ∧ rHeight < lowestZ → putTileState off water
                  | z ≤ rHeight               → putTileState off gravel
                  | otherwise                 → return ()
-------------------------------------------------------------------------------

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

-- | Find the closest matching biome generator for a given climate.
chooseBiomeForClimate ∷ Climate → BiomeSet → SomeBiomeGen
chooseBiomeForClimate cli biomes
  = snd $ foldl' f ((1/0), error "impossible: no biomes") biomes
  where
    f ∷ (Float, SomeBiomeGen) → SomeBiomeGen → (Float, SomeBiomeGen)
    f best@(shortestDistSq, _) biome
      = let cli'   = withBiomeGenProxy biome climate
            pow2 d = d⋅d
            distSq = pow2 (cliTemperature cli - cliTemperature cli') +
                     pow2 (cliHumidity    cli - cliHumidity    cli') +
                     pow2 (cliAltitude    cli - cliAltitude    cli')
        in
          if distSq < shortestDistSq then
            (distSq, biome)
          else
            best

loadBiomes ∷ ∀r. (Member (State LoaderContext) r, MonadThrow (Eff r)) ⇒ Eff r ()
loadBiomes
  = do traverse_ register' climateBasedBiomeProxies
       registerBiome (Proxy ∷ Proxy Ocean)
       registerBiome (Proxy ∷ Proxy River)
  where
    register' ∷ SomeBiomeProxy → Eff r ()
    register' (SomeBiomeProxy bp)
      = registerBiome bp
