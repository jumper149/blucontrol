module Bludigon.Recolor (
  MonadRecolor (..)
) where

import Bludigon.RGB

class Monad m => MonadRecolor m where
  recolor :: Trichromaticity -> m ()
