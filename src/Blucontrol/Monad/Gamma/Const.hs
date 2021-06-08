{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Monad.Gamma.Const (
  GammaConstT
, runGammaConstT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Reader

import Blucontrol.Monad.Gamma

newtype GammaConstT c m a = GammaConstT { unGammaConstT :: ReaderT c m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance MonadReader r m => MonadReader r (GammaConstT c m) where
  ask = lift ask
  local f tma = liftWith $ \ run ->
    local f $ run tma

instance Monad m => MonadGamma (GammaConstT c m) where
  type GammaValue (GammaConstT c m) = c
  gamma = GammaConstT ask

runGammaConstT :: c -> GammaConstT c m a -> m a
runGammaConstT !rgb tma = runReaderT (unGammaConstT tma) rgb