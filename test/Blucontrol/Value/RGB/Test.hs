{-# LANGUAGE RecordWildCards #-}

module Blucontrol.Value.RGB.Test (
  test
, Arbitrary_RGBWord8 (..)
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
    property $ total @Arbitrary_RGBWord8

newtype Arbitrary_RGBWord8 = Arbitrary_RGBWord8 (RGB Word8)
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance NFData Arbitrary_RGBWord8

instance Arbitrary Arbitrary_RGBWord8 where
  arbitrary = elements [minBound .. maxBound]
