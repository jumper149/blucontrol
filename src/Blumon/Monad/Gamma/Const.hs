{-# LANGUAGE UndecidableInstances #-}

module Blumon.Monad.Gamma.Const (
  GammaConstT
, runGammaConstT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Reader

import Blumon.Monad.Gamma
import Blumon.RGB

newtype GammaConstT m a = GammaConstT { unGammaConstT :: ReaderT Trichromaticity m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance Monad m => MonadGamma (GammaConstT m) where
  gamma = GammaConstT ask

instance MonadReader r m => MonadReader r (GammaConstT m) where
  ask = lift ask
  local f tma = liftWith $ \ run ->
    local f $ run tma

runGammaConstT :: Trichromaticity -> GammaConstT m a -> m a
runGammaConstT rgb tma = runReaderT (unGammaConstT tma) rgb
