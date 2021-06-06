{-# LANGUAGE RecordWildCards #-}

module Blucontrol.Test.Gamma.Linear (
  test
) where

import Test.Hspec
import Test.QuickCheck

import Control.DeepSeq
import Control.Monad.Identity
import Data.Time
import Data.Word
import GHC.Generics

import Blucontrol.Gamma.Linear
import Blucontrol.RGB
import Blucontrol.Test.RGB (Arbitrary_RGBWord8 (..))

test :: Spec
test = describe "Blucontrol.Gamma.Linear" $ do

  it "convert Time to TimeOfDay" $
    property prop_timeToTimeOfDay

  -- TODO: this tests `calculateValue weightedAverageRGB`
  it "calculateRGB between surrounding values" $
    property prop_calculateRGB

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
        TimeOfDay {..} = fst $ time Blucontrol.Gamma.Linear.==> (undefined :: RGB Word8)

prop_calculateRGB :: Arbitrary_Time
                  -> (Arbitrary_Time,Arbitrary_RGBWord8)
                  -> (Arbitrary_Time,Arbitrary_RGBWord8)
                  -> Bool
prop_calculateRGB (Arbitrary_Time time) (Arbitrary_Time xt , Arbitrary_RGBWord8 xtc) (Arbitrary_Time yt , Arbitrary_RGBWord8 ytc) =
  rgb `prop_RGBBetween` (xtc , ytc)
  where rgb = runIdentity . runGammaLinearT rgbMap $ calculateValue weightedAverageRGB tod
        rgbMap = xt Blucontrol.Gamma.Linear.==> xtc
            :| [ yt Blucontrol.Gamma.Linear.==> ytc
               ]
        tod = LocalTime (ModifiedJulianDay 0) . fst $ time Blucontrol.Gamma.Linear.==> (undefined :: RGB Word8)

prop_RGBBetween :: RGB Word8 -> (RGB Word8,RGB Word8) -> Bool
prop_RGBBetween x (a,b) = and
  [ red x `prop_between` (red a , red b)
  , green x `prop_between` (green a , green b)
  , blue x `prop_between` (blue a , blue b)
  ]

prop_between :: Ord a => a -> (a,a) -> Bool
prop_between x (a,b) = x <= max a b && x >= min a b
