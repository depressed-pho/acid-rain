{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Game.AcidRain.World.Climate
  ( Climate(..)
  ) where

import Data.Default (Default(..))
import Data.Hashable (Hashable)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
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

-- | The climate vector.
newtype instance UV.MVector σ Climate = MV_Cli (UV.MVector σ (Float, Float, Float))
newtype instance UV.Vector    Climate = V_Cli  (UV.Vector    (Float, Float, Float))

instance GMV.MVector UV.MVector Climate where
  basicLength (MV_Cli v) = GMV.basicLength v
  basicUnsafeSlice s l (MV_Cli v) = MV_Cli $ GMV.basicUnsafeSlice s l v
  basicOverlaps (MV_Cli v) (MV_Cli v') = GMV.basicOverlaps v v'
  basicUnsafeNew l = MV_Cli <$> GMV.basicUnsafeNew l
  basicInitialize (MV_Cli v) = GMV.basicInitialize v
  basicUnsafeRead (MV_Cli v) i
    = do (temp, hum, alt) ← GMV.basicUnsafeRead v i
         return $ Climate temp hum alt
  basicUnsafeWrite (MV_Cli v) i cli
    = GMV.basicUnsafeWrite v i (cliTemperature cli, cliHumidity cli, cliAltitude cli)

instance GV.Vector UV.Vector Climate where
  basicUnsafeFreeze (MV_Cli v) = V_Cli <$> GV.basicUnsafeFreeze v
  basicUnsafeThaw (V_Cli v) = MV_Cli <$> GV.basicUnsafeThaw v
  basicLength (V_Cli v) = GV.basicLength v
  basicUnsafeSlice s l (V_Cli v) = V_Cli $ GV.basicUnsafeSlice s l v
  basicUnsafeIndexM (V_Cli v) i
    = do (temp, hum, alt) ← GV.basicUnsafeIndexM v i
         return $ Climate temp hum alt
