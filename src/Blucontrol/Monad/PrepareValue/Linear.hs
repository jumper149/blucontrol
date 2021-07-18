{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Monad.PrepareValue.Linear (
  PrepareValueLinearT
, runPrepareValueLinearT
, Time (..)
, Hour
, Minute
, (==>)
, N.NonEmpty (..) -- TODO: keep here?
, calculateValue -- TODO: export for testing
, weightedAverageRGB -- TODO: export for testing
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
import Data.Word
import GHC.Generics

import Blucontrol.Monad.PrepareValue
import Blucontrol.Value.Brightness
import Blucontrol.Value.RGB
import Blucontrol.Value.RGB.Temperature

newtype PrepareValueLinearT c m a = PrepareValueLinearT { unPrepareValueLinearT :: ReaderT (M.Map TimeOfDay c) m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance MonadBase IO m => MonadPrepareValue (PrepareValueLinearT (RGB Word8) m) where
  type PreparedValue (PrepareValueLinearT (RGB Word8) m) = RGB Word8
  preparedValue = calculateValue weightedAverageRGB . zonedTimeToLocalTime =<< liftBase getZonedTime

instance MonadBase IO m => MonadPrepareValue (PrepareValueLinearT Temperature m) where
  type PreparedValue (PrepareValueLinearT Temperature m) = Temperature
  preparedValue = calculateValue weightedAverageTemperature . zonedTimeToLocalTime =<< liftBase getZonedTime

instance (MonadBase IO m, MonadPrepareValue (PrepareValueLinearT c m)) => MonadPrepareValue (PrepareValueLinearT (WithBrightness c) m) where
  type PreparedValue (PrepareValueLinearT (WithBrightness c) m) = WithBrightness (PreparedValue (PrepareValueLinearT c m))
  -- TODO: It would be nice to use the same exact time for `color'` and `brightness'`.
  preparedValue = do
    color' <- withPrepareValueLinearT color preparedValue
    brightness' <- withPrepareValueLinearT brightness $ calculateValue weightedAverageBrightness . zonedTimeToLocalTime =<< liftBase getZonedTime
    return WithBrightness { brightness = brightness'
                          , color = color'
                          }

withPrepareValueLinearT :: (c' -> c) -> PrepareValueLinearT c m a -> PrepareValueLinearT c' m a
withPrepareValueLinearT f m = PrepareValueLinearT $ withReaderT (fmap f) $ unPrepareValueLinearT m

nextTimeValue :: M.Map TimeOfDay c -> LocalTime -> Maybe (LocalTime,c)
nextTimeValue m time = catchError (toLocalTimeToday <$> M.lookupGT (localTimeOfDay time) m) $
                     const (toLocalTimeTomorrow <$> M.lookupMin m)
  where toLocalTimeToday (tod,tc) = let t = LocalTime { localDay = localDay time
                                                      , localTimeOfDay = tod
                                                      }
                                     in (t,tc)
        toLocalTimeTomorrow x = let (t,tc) = toLocalTimeToday x
                                    t' = t { localDay = succ $ localDay t }
                                 in (t',tc)

prevTimeValue :: M.Map TimeOfDay c -> LocalTime -> Maybe (LocalTime,c)
prevTimeValue m time = catchError (toLocalTimeToday <$> M.lookupLE (localTimeOfDay time) m) $
                     const (toLocalTimeYesterday <$> M.lookupMax m)
  where toLocalTimeToday (tod,tc) = let t = LocalTime { localDay = localDay time
                                                      , localTimeOfDay = tod
                                                      }
                                     in (t,tc)
        toLocalTimeYesterday x = let (t,tc) = toLocalTimeToday x
                                     t' = t { localDay = pred $ localDay t }
                                  in (t',tc)

calculateValue :: Monad m
               => (Rational -> c -> c -> c)
               -> LocalTime -> PrepareValueLinearT c m c
calculateValue weightedAverage time = do
  m <- PrepareValueLinearT ask
  return . fromJust $ do
    (nextTime , nextValue) <- nextTimeValue m time
    (prevTime , prevValue) <- prevTimeValue m time
    let diffSeconds t1 t2 = nominalDiffTimeToSeconds $ t1 `diffLocalTime` t2
        timeFraction = toRational $ (time `diffSeconds` prevTime) / (nextTime `diffSeconds` prevTime)
    return $ weightedAverage timeFraction prevValue nextValue

weightedAverageRGB :: Rational -> RGB Word8 -> RGB Word8 -> RGB Word8
weightedAverageRGB w rgb1 rgb2 = RGB { red = f (red rgb1) (red rgb2)
                                     , green = f (green rgb1) (green rgb2)
                                     , blue = f (blue rgb1) (blue rgb2)
                                     }
  where f c1 c2 = round $ fromIntegral c1 + w * (fromIntegral c2 - fromIntegral c1)

weightedAverageTemperature :: Rational -> Temperature -> Temperature -> Temperature
weightedAverageTemperature w t1 t2 = fromRational $ toRational t1 + w * (toRational t2 - toRational t1)

weightedAverageBrightness :: Rational -> Brightness -> Brightness -> Brightness
weightedAverageBrightness w b1 b2 = fromRational $ toRational b1 + w * (toRational b2 - toRational b1)

runPrepareValueLinearT' :: M.Map TimeOfDay c -> PrepareValueLinearT c m a -> m a
runPrepareValueLinearT' !rgbs tma = runReaderT (unPrepareValueLinearT tma) rgbs

runPrepareValueLinearT :: N.NonEmpty (TimeOfDay,c) -> PrepareValueLinearT c m a -> m a
runPrepareValueLinearT rgbs = runPrepareValueLinearT' $ M.fromList . N.toList $ rgbs

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

infix 6 ==>
(==>) :: Time -> c -> (TimeOfDay,c)
(==>) (h :. m) c = (time,c)
  where time = TimeOfDay { todHour = fromIntegral h
                         , todMin = fromIntegral m
                         , todSec = 0
                         }
