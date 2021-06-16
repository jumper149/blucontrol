{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Value.RGB.Temperature (
  Temperature
) where

import Control.DeepSeq
import Data.Default
import Data.Word
import GHC.Generics

import Blucontrol.Value
import Blucontrol.Value.RGB

-- | Arbitrary precision temperature in Kelvin
newtype Temperature = Temperature Rational
  deriving (Enum, Eq, Fractional, Generic, Num, Ord, Read, Real, RealFrac, Show)

instance NFData Temperature

instance Bounded Temperature where
  minBound = 0
  maxBound = 20000

instance Default Temperature where
  def = 6600

instance CompatibleValues (RGB Word8) a => CompatibleValues Temperature a where
  -- TODO: Test and implement more accurately. Currently based on blugon.
  convertValue = convertValue . toRGBWord8
    where toRGBWord8 :: Temperature -> RGB Word8
          toRGBWord8 (Temperature temp) = RGB { red, green, blue }
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
                  inBounds x
                    | x < 0 = 0
                    | x > 255 = 255
                    | otherwise = x
