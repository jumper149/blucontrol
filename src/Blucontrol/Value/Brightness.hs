module Blucontrol.Value.Brightness (
  Brightness
, WithBrightness (..)
) where

import Control.DeepSeq
import Data.Default
import GHC.Generics

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

-- TODO: Maybe allow applying to RGB?
--toRGB WithBrightness {..} = mapRGB applyBrightness $ toRGB rgb
--  where applyBrightness = truncate . (toRational brightness *) . toRational
