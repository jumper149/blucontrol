module Bludigon.Recolor (
  MonadRecolor (..)
) where

import Bludigon.RGB

class Monad m => MonadRecolor m where

  -- | Apply a 'Trichromaticity'.
  recolor :: Trichromaticity -> m ()
