{-# LANGUAGE RecordWildCards #-}

module Blumon.Test.RGB (
  test
) where

import Test.Hspec
import Test.QuickCheck

import Control.DeepSeq
import Data.Time
import GHC.Generics

import Blumon.RGB

test :: Spec
test = describe "Blumon.RGB" $ do

  it "Chromaticity in bounds" $
    property prop_Chromaticity

  it "Trichromaticity in bounds" $
    property prop_Trichromaticity

newtype Arbitrary_Chromaticity = Arbitrary_Chromaticity Chromaticity
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Read, Show)

instance Arbitrary Arbitrary_Chromaticity where
  arbitrary = Arbitrary_Chromaticity <$> elements [minBound .. maxBound]

prop_Chromaticity :: Arbitrary_Chromaticity -> ()
prop_Chromaticity = rnf

newtype Arbitrary_Trichromaticity = Arbitrary_Trichromaticity Trichromaticity
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Read, Show)

instance Arbitrary Arbitrary_Trichromaticity where
  arbitrary = Arbitrary_Trichromaticity <$> elements [minBound .. maxBound]

prop_Trichromaticity :: Arbitrary_Trichromaticity -> ()
prop_Trichromaticity = rnf
