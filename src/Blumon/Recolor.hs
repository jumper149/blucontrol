module Blumon.Recolor (
  MonadRecolor (..)
) where

import Blumon.Gamma

class MonadGamma m => MonadRecolor m where
  recolor :: m ()
