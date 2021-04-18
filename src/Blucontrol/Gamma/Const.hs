{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Gamma.Const (
  GammaConstT
, runGammaConstT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Reader

import Blucontrol.Gamma
import Blucontrol.RGB

newtype GammaConstT c m a = GammaConstT { unGammaConstT :: ReaderT c m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance MonadReader r m => MonadReader r (GammaConstT c m) where
  ask = lift ask
  local f tma = liftWith $ \ run ->
    local f $ run tma

instance (Monad m, RGB c) => MonadGamma (GammaConstT c m) where
  gamma = toRGB <$> GammaConstT ask

runGammaConstT :: RGB c => c -> GammaConstT c m a -> m a
runGammaConstT rgb tma = runReaderT (unGammaConstT tma) rgb
