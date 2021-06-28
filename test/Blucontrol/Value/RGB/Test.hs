module Blucontrol.Value.RGB.Test (
  test
, RGBWord8Arbitrary (..)
) where

import Test.Hspec
import Test.QuickCheck

import Control.DeepSeq
import Data.Word
import GHC.Generics

import Blucontrol.Value.RGB

test :: Spec
test = describe "Blucontrol.RGB" $ do

  it "(RGB Word8) in bounds." $
    property $ total @RGBWord8Arbitrary

newtype RGBWord8Arbitrary = RGBWord8Arbitrary (RGB Word8)
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance NFData RGBWord8Arbitrary

instance Arbitrary RGBWord8Arbitrary where
  arbitrary = elements [minBound .. maxBound]
