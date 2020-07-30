module Blumon.Recolor (
  MonadRecolor (..)
) where

import Blumon.RGB

class Monad m => MonadRecolor m where
  recolor :: Trichromaticity -> m ()
