module Blucontrol.Value.Brightness (
  Brightness
, WithBrightness (..)
, applyBrightnessToRGB
) where

import Control.DeepSeq
import Data.Default
import GHC.Generics

import Blucontrol.Value
import Blucontrol.Value.RGB

-- | Arbitrary precision brightness between 0 and 1
newtype Brightness = Brightness Rational
  deriving (Enum, Eq, Fractional, Generic, Num, Ord, Read, Real, RealFrac, Show)

instance NFData Brightness

instance Bounded Brightness where
  minBound = 0
  maxBound = 1

instance Default Brightness where
  def = 1

-- | Combination of a color value and a 'Brightness'
data WithBrightness a = WithBrightness { brightness :: Brightness
                                       , color :: a
                                       }
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData a => NFData (WithBrightness a)

instance Default a => Default (WithBrightness a) where
  def = WithBrightness { brightness = def
                       , color = def
                       }

instance CompatibleValues a b => CompatibleValues a (WithBrightness b) where
  convertValue a = WithBrightness { brightness = def
                                  , color = convertValue a
                                  }

applyBrightnessToRGB :: (Integral a, Real a) => WithBrightness (RGB a) -> RGB a
applyBrightnessToRGB x = RGB { red = applyBrightness red'
                             , green = applyBrightness green'
                             , blue = applyBrightness blue'
                             }
  where applyBrightness = truncate . (toRational (brightness x) *) . toRational
        RGB { red = red', green = green', blue = blue' } = color x
