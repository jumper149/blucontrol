module Blumon.Gamma (
  MonadGamma (..)
) where

import Control.Monad.Except
import Control.Monad.Reader

import Blumon.RGB

class Monad m => MonadGamma m where
  gamma :: m Trichromaticity

instance MonadGamma m => MonadGamma (ReaderT r m) where
  gamma = lift gamma

instance MonadGamma m => MonadGamma (ExceptT e m) where
  gamma = lift gamma
