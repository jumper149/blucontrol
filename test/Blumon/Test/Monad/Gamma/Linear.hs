{-# LANGUAGE RecordWildCards #-}

module Blumon.Test.Monad.Gamma.Linear (
  test
) where

import Test.Hspec
import Test.QuickCheck

import Control.DeepSeq
import Control.Monad.Identity
import Data.Time
import GHC.Generics

import Blumon.Monad.Gamma.Linear
import Blumon.RGB
import Blumon.Test.RGB (Arbitrary_Trichromaticity (..))

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
prop_calculateGamma (Arbitrary_Time time) (Arbitrary_Time xt , Arbitrary_Trichromaticity xtc) (Arbitrary_Time yt , Arbitrary_Trichromaticity ytc) =
  rgb `prop_TrichromaticityBetween` (xtc , ytc)
  where rgb = runIdentity . runGammaLinearT rgbMap $ calculateGamma tod
        rgbMap = xt Blumon.Monad.Gamma.Linear.==> xtc
            :| [ yt Blumon.Monad.Gamma.Linear.==> ytc
               ]
        tod = LocalTime (ModifiedJulianDay 0) . fst $ time Blumon.Monad.Gamma.Linear.==> undefined

prop_TrichromaticityBetween :: Trichromaticity -> (Trichromaticity,Trichromaticity) -> Bool
prop_TrichromaticityBetween x (a,b) = and
  [ red x `prop_ChromaticityBetween` (red a , red b)
  , green x `prop_ChromaticityBetween` (green a , green b)
  , blue x `prop_ChromaticityBetween` (blue a , blue b)
  ]

prop_ChromaticityBetween :: Chromaticity -> (Chromaticity,Chromaticity) -> Bool
prop_ChromaticityBetween x (a,b) = x <= max a b && x >= min a b
