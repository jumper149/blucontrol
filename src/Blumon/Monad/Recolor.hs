module Blumon.Monad.Recolor (
  MonadRecolor (..)
) where

import Blumon.Monad.Gamma

class MonadGamma m => MonadRecolor m where
  recolor :: m ()
