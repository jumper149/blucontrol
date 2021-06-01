{-# LANGUAGE RecordWildCards #-}

module Blucontrol.RGB.Brightness (
  Brightness
, WithBrightness (..)
) where

import Control.DeepSeq
import Data.Default
import GHC.Generics

import Blucontrol.RGB

-- | Arbitrary precision brightness between 0 and 1
newtype Brightness = Brightness Rational
  deriving (Enum, Eq, Fractional, Generic, Num, Ord, Read, Real, RealFrac, Show)

instance NFData Brightness

instance Bounded Brightness where
  minBound = 0
  maxBound = 1

instance Default Brightness where
  def = 1

-- | Parametric RGB value with an associated brightness
data WithBrightness a = WithBrightness { brightness :: Brightness
                                       , rgb :: a
                                       }
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData a => NFData (WithBrightness a)

instance Default a => Default (WithBrightness a) where
  def = WithBrightness { brightness = def
                       , rgb = def
                       }

-- TODO: maybe create separate instances for Trichromaticity and Temperature
instance RGB a => RGB (WithBrightness a) where
  toRGB WithBrightness {..} = mapChromaticity applyBrightness $ toRGB rgb
      where applyBrightness = truncate . (toRational brightness *) . toRational

mapChromaticity :: (Chromaticity -> Chromaticity) -> Trichromaticity -> Trichromaticity
mapChromaticity f rgb = Trichromaticity { red = f $ red rgb
                                        , green = f $ green rgb
                                        , blue = f $ blue rgb
                                        }
