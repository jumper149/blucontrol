module Blumon.Monad.Gamma (
  MonadGamma (..)
) where

import Control.Monad.Trans
import Control.Monad.Reader

import Blumon.RGB

class Monad m => MonadGamma m where
  gamma :: m Trichromaticity

instance MonadGamma m => MonadGamma (ReaderT r m) where
  gamma = lift gamma
