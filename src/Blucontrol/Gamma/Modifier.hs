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

newtype GammaModifierT m a = GammaModifierT { unGammaModifierT :: ReaderT (GammaRGB m -> IO (GammaRGB m)) m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)
-- TODO: A `MonadTransControl` instance seems to be impossible

instance MonadTrans GammaModifierT where
  lift = GammaModifierT . lift

instance (MonadBase IO m, MonadGamma m) => MonadGamma (GammaModifierT m) where
  type GammaRGB (GammaModifierT m) = GammaRGB m
  gamma = do oldGamma <- lift gamma
             modifyGamma <- GammaModifierT ask
             liftBase $ modifyGamma oldGamma

runGammaModifierT :: (GammaRGB m -> IO (GammaRGB m)) -> GammaModifierT m a -> m a
runGammaModifierT modify tma = runReaderT (unGammaModifierT tma) modify
