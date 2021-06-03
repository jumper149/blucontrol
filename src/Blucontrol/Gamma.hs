module Blucontrol.Gamma (
  MonadGamma (..)
) where

import Blucontrol.RGB

class (Monad m, RGB (GammaRGB m)) => MonadGamma m where

  type GammaRGB m

  {- | Calculate an 'RGB' value.
     This is a monadic function, to allow the value to be dependent on side effects like time and
     location.
  -}
  gamma :: m (GammaRGB m)
