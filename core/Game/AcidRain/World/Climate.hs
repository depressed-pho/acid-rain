{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Climate
  ( Climate(..)
  ) where

import Data.Default (Default(..))
import Data.Hashable (Hashable)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import GHC.Generics (Generic)


-- | Climate is defined for each @(x, y)@ world position in the
-- world. It is mainly used to choose a biome, but it may also affect
-- some other mechanics.
data Climate
  = Climate
    { -- | Temperature in Celsius @[-273.15, ∞)@.
      cliTemperature ∷ {-# UNPACK #-} !Float
      -- | Relative humidity in @[0, 1]@.
    , cliHumidity    ∷ {-# UNPACK #-} !Float
      -- | Altidude in meters where @0@ is the sea level.
    , cliAltitude    ∷ {-# UNPACK #-} !Float
    } deriving (Show, Eq, Generic)

instance Hashable Climate
instance Default Climate where
  def = Climate
        { cliTemperature = 14.88 -- Global average temperature in 2020.
        , cliHumidity    = 0.7   -- 70% RH would be a reasonable default.
        , cliAltitude    = 0     -- We don't use this default value anyway.
        }

-- Derive unboxed vectors for Climate.
derivingUnbox "Cli"
  [t| Climate → (Float, Float, Float) |]
  [e| \cli              → (cliTemperature cli, cliHumidity cli, cliAltitude cli) |]
  [e| \(temp, hum, alt) → Climate temp hum alt                                   |]
