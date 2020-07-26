{-# LANGUAGE RecordWildCards #-}

module Blumon.Test.Monad.Gamma.Linear (
  test
) where

import Test.Hspec
import Test.QuickCheck

import Control.DeepSeq
import Data.Time
import GHC.Generics

import Blumon.Monad.Gamma.Linear
import Blumon.Test.RGB hiding (test)

test :: Spec
test = describe "Blumon.Monad.Gamma.Linear" $ do

  it "convert Time to TimeOfDay" $
    property prop_timeToTimeOfDay

  it "calculateGamma between surrounding values" $
    property prop_calculateGamma

newtype Arbitrary_Time = Arbitrary_Time Time
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance NFData Arbitrary_Time

instance Arbitrary Arbitrary_Time where
  arbitrary = elements [minBound .. maxBound]

prop_timeToTimeOfDay :: Arbitrary_Time -> Bool
prop_timeToTimeOfDay (Arbitrary_Time time) = and
  [ fromIntegral h == todHour
  , fromIntegral m == todMin
  , 0 == todSec
  ]
  where h :. m = time
        TimeOfDay {..} = fst $ time Blumon.Monad.Gamma.Linear.==> undefined

prop_calculateGamma :: Arbitrary_Time
                    -> (Arbitrary_Time,Arbitrary_Trichromaticity)
                    -> (Arbitrary_Time,Arbitrary_Trichromaticity)
                    -> Bool
prop_calculateGamma = undefined 
