module Blucontrol.Gamma (
  MonadGamma (..)
) where

class Monad m => MonadGamma m where

  type GammaValue m

  {- | Calculate a gamma value.
     This is a monadic function, to allow the value to be dependent on side effects like time and
     location.
  -}
  gamma :: m (GammaValue m)
