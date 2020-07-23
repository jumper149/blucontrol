module Blumon.Monad.Gamma.Const (
  GammaConstT
, runGammaConstT
) where

import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Blumon.Monad.Gamma
import Blumon.RGB

newtype GammaConstT m a = GammaConstT { unGammaConstT :: ReaderT Trichromaticity m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadTrans)

instance Monad m => MonadGamma (GammaConstT m) where
  gamma = GammaConstT ask

runGammaConstT :: Trichromaticity -> GammaConstT m a -> m a
runGammaConstT rgb tma = runReaderT (unGammaConstT tma) rgb
