{-# LANGUAGE ScopedTypeVariables #-}

module Blucontrol.Value.RGB (
  RGB (..)
) where

import Control.DeepSeq
import Data.Default
import Data.Word
import GHC.Generics

-- | Values for the colors 'red', 'green' and 'blue'
data RGB a = RGB { red :: a
                 , green :: a
                 , blue :: a
                 }
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance NFData a => NFData (RGB a)

-- TODO: Is this instance really necessary?
instance (Bounded a, Enum a) => Enum (RGB a) where
  fromEnum rgb = sum [ fromEnum (red rgb)
                     , fromEnum (green rgb) * range
                     , fromEnum (blue rgb) * range * range
                     ]
    where range = succ . fromEnum $ maxBound @a
  toEnum i = let (b , i') = i `divMod` (range * range)
                 (g , r) = i' `divMod` range
              in RGB { red = toEnum r
                     , green = toEnum g
                     , blue = toEnum b
                     }
    where range = succ . fromEnum $ maxBound @a

instance Default (RGB Word8) where
  def = RGB { red = maxBound
            , green = maxBound
            , blue = maxBound
            }
