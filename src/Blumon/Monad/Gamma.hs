module Blumon.Monad.Gamma (
  MonadGamma (..)
) where

import Blumon.RGB

class Monad m => MonadGamma m where
  gamma :: m Trichromaticity
