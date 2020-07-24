module Blumon.RGB (
  Chromaticity
, Trichromaticity (..)
) where

import Data.Default
import Data.Word
import GHC.Generics

-- | 8-bit value for color channel intensity
newtype Chromaticity = Chromaticity Word8
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

instance Default Chromaticity where
  def = maxBound

-- | combination of 'Chromaticity's for the colors 'red', 'green' and 'blue'
data Trichromaticity = Trichromaticity { red :: Chromaticity
                                       , green :: Chromaticity
                                       , blue :: Chromaticity
                                       }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default Trichromaticity where
  def = Trichromaticity { red = def
                        , green = def
                        , blue = def
                        }
