module Blucontrol.Recolor (
  MonadRecolor (..)
) where

import Blucontrol.RGB

class (Monad m, RGB (RecolorRGB m)) => MonadRecolor m where

  type RecolorRGB m

  -- | Apply a 'Trichromaticity'.
  recolor :: RecolorRGB m -> m ()
