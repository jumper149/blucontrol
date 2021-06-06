module Blucontrol.Gamma (
  MonadGamma (..)
) where

class Monad m => MonadGamma m where

  type GammaRGB m

  {- | Calculate an gamma value.
     This is a monadic function, to allow the value to be dependent on side effects like time and
     location.
  -}
  gamma :: m (GammaRGB m)
