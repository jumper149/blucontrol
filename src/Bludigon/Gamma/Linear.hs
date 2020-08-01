{-# LANGUAGE UndecidableInstances #-}

module Bludigon.Gamma.Linear (
  GammaLinearT
, runGammaLinearT
, Time (..)
, Hour
, Minute
, (==>)
, N.NonEmpty (..) -- TODO: keep here?
, calculateGamma -- TODO: export for testing
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

import Bludigon.Gamma
import Bludigon.RGB

newtype GammaLinearT m a = GammaLinearT { unGammaLinearT :: ReaderT (M.Map TimeOfDay Trichromaticity) m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance MonadBase IO m => MonadGamma (GammaLinearT m) where
  gamma = calculateGamma . zonedTimeToLocalTime =<< liftBase getZonedTime

calculateGamma :: Monad m => LocalTime -> GammaLinearT m Trichromaticity
calculateGamma time = do
  m <- GammaLinearT ask
  return . fromJust $ do
    (nextTime , nextGamma) <- nextTimeGamma m time
    (prevTime , prevGamma) <- prevTimeGamma m time
    let diffSeconds t1 t2 = nominalDiffTimeToSeconds $ t1 `diffLocalTime` t2
        timeFraction = toRational $ (time `diffSeconds` prevTime) / (nextTime `diffSeconds` prevTime)
    return $ weightedAverage timeFraction prevGamma nextGamma

nextTimeGamma :: M.Map TimeOfDay Trichromaticity -> LocalTime -> Maybe (LocalTime,Trichromaticity)
nextTimeGamma m time = catchError (toLocalTimeToday <$> M.lookupGT (localTimeOfDay time) m) $
                         const (toLocalTimeTomorrow <$> M.lookupMin m)
  where toLocalTimeToday (tod,tc) = let t = LocalTime { localDay = localDay time
                                                      , localTimeOfDay = tod
                                                      }
                                     in (t,tc)
        toLocalTimeTomorrow x = let (t,tc) = toLocalTimeToday x
                                    t' = t { localDay = succ $ localDay t }
                                 in (t',tc)

prevTimeGamma :: M.Map TimeOfDay Trichromaticity -> LocalTime -> Maybe (LocalTime,Trichromaticity)
prevTimeGamma m time = catchError (toLocalTimeToday <$> M.lookupLE (localTimeOfDay time) m) $
                         const (toLocalTimeYesterday <$> M.lookupMax m)
  where toLocalTimeToday (tod,tc) = let t = LocalTime { localDay = localDay time
                                                      , localTimeOfDay = tod
                                                      }
                                     in (t,tc)
        toLocalTimeYesterday x = let (t,tc) = toLocalTimeToday x
                                     t' = t { localDay = pred $ localDay t }
                                  in (t',tc)

weightedAverage :: Rational -> Trichromaticity -> Trichromaticity -> Trichromaticity
weightedAverage w tc1 tc2 = Trichromaticity { red = f (red tc1) (red tc2)
                                            , green = f (green tc1) (green tc2)
                                            , blue = f (blue tc1) (blue tc2)
                                            }
  where f c1 c2 = round $ fromIntegral c1 + w * (fromIntegral c2 - fromIntegral c1)

instance MonadReader r m => MonadReader r (GammaLinearT m) where
  ask = lift ask
  local f tma = liftWith $ \ run ->
    local f $ run tma

runGammaLinearT' :: M.Map TimeOfDay Trichromaticity -> GammaLinearT m a -> m a
runGammaLinearT' rgbs tma = runReaderT (unGammaLinearT tma) rgbs

runGammaLinearT :: N.NonEmpty (TimeOfDay,Trichromaticity) -> GammaLinearT m a -> m a
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

infix 6 ==>
(==>) :: Time -> Trichromaticity -> (TimeOfDay,Trichromaticity)
(==>) (h :. m) c = (time,c)
  where time = TimeOfDay { todHour = fromIntegral h
                         , todMin = fromIntegral m
                         , todSec = 0
                         }
