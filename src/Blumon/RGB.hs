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
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance Enum Trichromaticity where
  fromEnum tc = sum [ fromEnum (red tc)
                    , fromEnum (green tc) * range
                    , fromEnum (blue tc) * range * range
                    ]
    where range = succ . fromEnum $ maxBound @Chromaticity
  toEnum i = let (b , i') = i `divMod` (range * range)
                 (g , r) = i' `divMod` range
              in Trichromaticity { red = toEnum r
                                 , green = toEnum g
                                 , blue = toEnum b
                                 }
    where range = succ . fromEnum $ maxBound @Chromaticity

instance Default Trichromaticity where
  def = Trichromaticity { red = def
                        , green = def
                        , blue = def
                        }
