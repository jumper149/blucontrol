{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Gamma.Linear (
  GammaLinearT
, runGammaLinearT
, Time (..)
, Hour
, Minute
, (==>)
, N.NonEmpty (..) -- TODO: keep here?
, calculateRGB -- TODO: export for testing
, weightedAverageTrichromaticity -- TODO: export for testing
) where

import Control.DeepSeq
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Data.Finite as F
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Time
import GHC.Generics

import Blucontrol.Gamma
import Blucontrol.RGB
import Blucontrol.RGB.Brightness
import Blucontrol.RGB.Temperature

newtype GammaLinearT c m a = GammaLinearT { unGammaLinearT :: ReaderT (M.Map TimeOfDay c) m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance MonadReader r m => MonadReader r (GammaLinearT c m) where
  ask = lift ask
  local f tma = liftWith $ \ run ->
    local f $ run tma

instance MonadBase IO m => MonadGamma (GammaLinearT Trichromaticity m) where
  type GammaRGB (GammaLinearT Trichromaticity m) = Trichromaticity
  gamma = calculateRGB weightedAverageTrichromaticity . zonedTimeToLocalTime =<< liftBase getZonedTime

instance MonadBase IO m => MonadGamma (GammaLinearT Temperature m) where
  type GammaRGB (GammaLinearT Temperature m) = Temperature
  gamma = calculateRGB weightedAverageTemperature . zonedTimeToLocalTime =<< liftBase getZonedTime

instance (RGB Trichromaticity, MonadBase IO m) => MonadGamma (GammaLinearT (WithBrightness Trichromaticity) m) where
  type GammaRGB (GammaLinearT (WithBrightness Trichromaticity) m) = WithBrightness Trichromaticity
  gamma = calculateRGB weightedAverage . zonedTimeToLocalTime =<< liftBase getZonedTime
    where weightedAverage w WithBrightness { brightness = b1, rgb = tc1 } WithBrightness { brightness = b2, rgb = tc2 } =
            WithBrightness { brightness = weightedAverageBrightness w b1 b2
                           , rgb = weightedAverageTrichromaticity w tc1 tc2
                           }

instance (RGB Temperature, MonadBase IO m) => MonadGamma (GammaLinearT (WithBrightness Temperature) m) where
  type GammaRGB (GammaLinearT (WithBrightness Temperature) m) = WithBrightness Temperature
  gamma = calculateRGB weightedAverage . zonedTimeToLocalTime =<< liftBase getZonedTime
    where weightedAverage w WithBrightness { brightness = b1, rgb = tc1 } WithBrightness { brightness = b2, rgb = tc2 } =
            WithBrightness { brightness = weightedAverageBrightness w b1 b2
                           , rgb = weightedAverageTemperature w tc1 tc2
                           }

nextTimeRGB :: M.Map TimeOfDay c -> LocalTime -> Maybe (LocalTime,c)
nextTimeRGB m time = catchError (toLocalTimeToday <$> M.lookupGT (localTimeOfDay time) m) $
                     const (toLocalTimeTomorrow <$> M.lookupMin m)
  where toLocalTimeToday (tod,tc) = let t = LocalTime { localDay = localDay time
                                                      , localTimeOfDay = tod
                                                      }
                                     in (t,tc)
        toLocalTimeTomorrow x = let (t,tc) = toLocalTimeToday x
                                    t' = t { localDay = succ $ localDay t }
                                 in (t',tc)

prevTimeRGB :: M.Map TimeOfDay c -> LocalTime -> Maybe (LocalTime,c)
prevTimeRGB m time = catchError (toLocalTimeToday <$> M.lookupLE (localTimeOfDay time) m) $
                     const (toLocalTimeYesterday <$> M.lookupMax m)
  where toLocalTimeToday (tod,tc) = let t = LocalTime { localDay = localDay time
                                                      , localTimeOfDay = tod
                                                      }
                                     in (t,tc)
        toLocalTimeYesterday x = let (t,tc) = toLocalTimeToday x
                                     t' = t { localDay = pred $ localDay t }
                                  in (t',tc)

calculateRGB :: Monad m
             => (Rational -> c -> c -> c)
             -> LocalTime -> GammaLinearT c m c
calculateRGB weightedAverage time = do
  m <- GammaLinearT ask
  return . fromJust $ do
    (nextTime , nextRGB) <- nextTimeRGB m time
    (prevTime , prevRGB) <- prevTimeRGB m time
    let diffSeconds t1 t2 = nominalDiffTimeToSeconds $ t1 `diffLocalTime` t2
        timeFraction = toRational $ (time `diffSeconds` prevTime) / (nextTime `diffSeconds` prevTime)
    return $ weightedAverage timeFraction prevRGB nextRGB

weightedAverageTrichromaticity :: Rational -> Trichromaticity -> Trichromaticity -> Trichromaticity
weightedAverageTrichromaticity w tc1 tc2 = Trichromaticity { red = f (red tc1) (red tc2)
                                                           , green = f (green tc1) (green tc2)
                                                           , blue = f (blue tc1) (blue tc2)
                                                           }
  where f c1 c2 = round $ fromIntegral c1 + w * (fromIntegral c2 - fromIntegral c1)

weightedAverageTemperature :: Rational -> Temperature -> Temperature -> Temperature
weightedAverageTemperature w t1 t2 = fromRational $ toRational t1 + w * (toRational t2 - toRational t1)

weightedAverageBrightness :: Rational -> Brightness -> Brightness -> Brightness
weightedAverageBrightness w b1 b2 = fromRational $ toRational b1 + w * (toRational b2 - toRational b1)

-- TODO: maybe remove RGB constraint
runGammaLinearT' :: RGB c => M.Map TimeOfDay c -> GammaLinearT c m a -> m a
runGammaLinearT' !rgbs tma = runReaderT (unGammaLinearT tma) rgbs

-- TODO: maybe remove RGB constraint
runGammaLinearT :: RGB c => N.NonEmpty (TimeOfDay,c) -> GammaLinearT c m a -> m a
runGammaLinearT rgbs = runGammaLinearT' $ M.fromList . N.toList $ rgbs

newtype Hour = Hour { unHour :: F.Finite 24 }
  deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Read, Real, Show)

instance NFData Hour

newtype Minute = Minute { unMinute :: F.Finite 60 }
  deriving (Bounded, Enum, Eq, Generic, Integral, Num, Ord, Read, Real, Show)

instance NFData Minute

infix 7 :.
data Time = Hour :. Minute
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance NFData Time

instance Enum Time where
  fromEnum (h :. m) = fromEnum h * succ (fromEnum $ maxBound @Minute) + fromEnum m
  toEnum i = let (h , m) = i `divMod` succ (fromEnum $ maxBound @Minute)
              in toEnum h :. toEnum m

-- TODO: maybe remove RGB constraint
infix 6 ==>
(==>) :: RGB c => Time -> c -> (TimeOfDay,c)
(==>) (h :. m) c = (time,c)
  where time = TimeOfDay { todHour = fromIntegral h
                         , todMin = fromIntegral m
                         , todSec = 0
                         }
