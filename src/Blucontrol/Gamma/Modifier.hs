{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Gamma.Modifier (
  GammaModifierT
, runGammaModifierT
) where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Blucontrol.Gamma

newtype GammaModifierT c m a = GammaModifierT { unGammaModifierT :: ReaderT (c -> c) m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance MonadGamma c m => MonadGamma c (GammaModifierT c m) where
  gamma = do oldGamma <- lift gamma
             modifyGamma <- GammaModifierT ask
             return $ modifyGamma oldGamma

instance MonadReader r m => MonadReader r (GammaModifierT c m) where
  ask = lift ask
  local f tma = liftWith $ \ run ->
    local f $ run tma

runGammaModifierT :: (c -> c) -> GammaModifierT c m a -> m a
runGammaModifierT modify tma = runReaderT (unGammaModifierT tma) modify
