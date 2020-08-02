{-# LANGUAGE RecordWildCards #-}

module Bludigon.RGB.Temperature (
  Temperature
, temperature
) where

import Control.DeepSeq
import Data.Default
import GHC.Generics

import Bludigon.RGB

-- | Arbitrary precision temperature in Kelvin
newtype Temperature = Temperature Rational
  deriving (Enum, Eq, Fractional, Generic, Num, Ord, Read, Real, RealFrac, Show)

instance NFData Temperature

instance Bounded Temperature where
  minBound = 0
  maxBound = 20000

instance Default Temperature where
  def = 6600

-- TODO: test and implement more accurate, currently based on blugon
-- | Calculate a 'Trichromaticity' from a 'Temperatute'.
temperature :: Temperature -> Trichromaticity
temperature (Temperature temp) = Trichromaticity {..}
  where red = round . inBounds $
          if t <= 66
             then 255
             else 329.698727446 * ((t - 60) ** (-0.1332047592))
        green = round . inBounds $
          if t <= 66
             then 99.4708025861 * log t - 161.1195681661
             else 288.1221695283 * ((t - 60) ** (-0.0755148492))
        blue = round . inBounds $
          if t <= 0
             then 0
             else if t >= 66
                     then 255
                     else 138.5177312231 * log (t - 10) - 305.0447927307
        t = fromRational $ temp / 100 :: Double
        inBounds x = if x < 0
                        then 0
                        else if x > 255
                             then 255
                             else x
