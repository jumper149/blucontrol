{-# LANGUAGE RecordWildCards #-}

module Blucontrol.Test.Gamma.Linear (
  test
) where

import Test.Hspec
import Test.QuickCheck

import Control.DeepSeq
import Control.Monad.Identity
import Data.Time
import GHC.Generics

import Blucontrol.Gamma.Linear
import Blucontrol.RGB
import Blucontrol.Test.RGB (Arbitrary_Trichromaticity (..))

test :: Spec
test = describe "Blucontrol.Gamma.Linear" $ do

  it "convert Time to TimeOfDay" $
    property prop_timeToTimeOfDay

  -- TODO: this tests `calculateRGB weightedAverageTrichromaticity`
  it "calculateTrichromaticity between surrounding values" $
    property prop_calculateTrichromaticity

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
        TimeOfDay {..} = fst $ time Blucontrol.Gamma.Linear.==> (undefined :: Trichromaticity)

prop_calculateTrichromaticity :: Arbitrary_Time
                              -> (Arbitrary_Time,Arbitrary_Trichromaticity)
                              -> (Arbitrary_Time,Arbitrary_Trichromaticity)
                              -> Bool
prop_calculateTrichromaticity (Arbitrary_Time time) (Arbitrary_Time xt , Arbitrary_Trichromaticity xtc) (Arbitrary_Time yt , Arbitrary_Trichromaticity ytc) =
  rgb `prop_TrichromaticityBetween` (xtc , ytc)
  where rgb = runIdentity . runGammaLinearT rgbMap $ calculateRGB weightedAverageTrichromaticity tod
        rgbMap = xt Blucontrol.Gamma.Linear.==> xtc
            :| [ yt Blucontrol.Gamma.Linear.==> ytc
               ]
        tod = LocalTime (ModifiedJulianDay 0) . fst $ time Blucontrol.Gamma.Linear.==> (undefined :: Trichromaticity)

prop_TrichromaticityBetween :: Trichromaticity -> (Trichromaticity,Trichromaticity) -> Bool
prop_TrichromaticityBetween x (a,b) = and
  [ red x `prop_ChromaticityBetween` (red a , red b)
  , green x `prop_ChromaticityBetween` (green a , green b)
  , blue x `prop_ChromaticityBetween` (blue a , blue b)
  ]

prop_ChromaticityBetween :: Chromaticity -> (Chromaticity,Chromaticity) -> Bool
prop_ChromaticityBetween x (a,b) = x <= max a b && x >= min a b
