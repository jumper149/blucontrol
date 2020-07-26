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
  if xtod <= tod && tod <= ytod
     then and [ smaller `prop_TrichromaticityLE` rgb
              , rgb `prop_TrichromaticityLE` larger
              ]
     else True
  where rgb = runIdentity . runGammaLinearT rgbMap $ calculateGamma tod
        rgbMap = xt Blumon.Monad.Gamma.Linear.==> xtc
            :| [ yt Blumon.Monad.Gamma.Linear.==> ytc
               ]
        tod = fst $ time Blumon.Monad.Gamma.Linear.==> undefined
        (smaller,larger) = case xtc `prop_TrichromaticityLE` ytc of
                             True -> (xtc , ytc)
                             False -> (xtc , xtc)
        xtod = fst $ xt Blumon.Monad.Gamma.Linear.==> undefined
        ytod = fst $ yt Blumon.Monad.Gamma.Linear.==> undefined


prop_TrichromaticityLE :: Trichromaticity -> Trichromaticity -> Bool
prop_TrichromaticityLE x y = and
  [ red x <= red y
  , green x <= green y
  , blue x <= blue y
  ]
