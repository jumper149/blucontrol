module Blucontrol.Recolor (
  MonadRecolor (..)
) where

import Blucontrol.RGB

class Monad m => MonadRecolor m where

  -- | Apply a 'Trichromaticity'.
  recolor :: Trichromaticity -> m ()
