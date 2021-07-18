module Blucontrol.Monad.PrepareValue.Linear.Test (
  test
) where

import Test.Hspec
import Test.QuickCheck

import Control.DeepSeq
import Control.Monad.Identity
import Data.Time
import Data.Word
import GHC.Generics

import Blucontrol.Monad.PrepareValue.Linear
import Blucontrol.Value.RGB
import Blucontrol.Value.RGB.Test (RGBWord8Arbitrary (..))

test :: Spec
test = describe "Blucontrol.PrepareValue.Linear" $ do

  it "convert Time to TimeOfDay" $
    property prop_timeToTimeOfDay

  -- TODO: this tests `calculateValue weightedAverageRGB`
  it "calculateRGB between surrounding values" $
    property prop_calculateRGB

newtype TimeArbitrary = TimeArbitrary Time
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance NFData TimeArbitrary

instance Arbitrary TimeArbitrary where
  arbitrary = elements [minBound .. maxBound]

prop_timeToTimeOfDay :: TimeArbitrary -> Bool
prop_timeToTimeOfDay (TimeArbitrary time) =
     (fromIntegral h == todHour)
  && (fromIntegral m == todMin)
  && (0 == todSec)
  where h :. m = time
        TimeOfDay { todHour, todMin, todSec } = fst $ time Blucontrol.Monad.PrepareValue.Linear.==> (undefined :: RGB Word8)

prop_calculateRGB :: TimeArbitrary
                  -> (TimeArbitrary, RGBWord8Arbitrary)
                  -> (TimeArbitrary, RGBWord8Arbitrary)
                  -> Bool
prop_calculateRGB (TimeArbitrary time) (TimeArbitrary xt , RGBWord8Arbitrary xtc) (TimeArbitrary yt , RGBWord8Arbitrary ytc) =
  rgb `prop_RGBBetween` (xtc , ytc)
  where rgb = runIdentity . runPrepareValueLinearT rgbMap $ calculateValue weightedAverageRGB tod
        rgbMap = xt Blucontrol.Monad.PrepareValue.Linear.==> xtc
            :| [ yt Blucontrol.Monad.PrepareValue.Linear.==> ytc
               ]
        tod = LocalTime (ModifiedJulianDay 0) . fst $ time Blucontrol.Monad.PrepareValue.Linear.==> (undefined :: RGB Word8)

prop_RGBBetween :: RGB Word8 -> (RGB Word8,RGB Word8) -> Bool
prop_RGBBetween x (a,b) =
     (red x `prop_between` (red a , red b))
  && (green x `prop_between` (green a , green b))
  && (blue x `prop_between` (blue a , blue b))

prop_between :: Ord a => a -> (a,a) -> Bool
prop_between x (a,b) = x <= max a b && x >= min a b
