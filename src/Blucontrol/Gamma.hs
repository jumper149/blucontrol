module Blucontrol.Gamma (
  MonadGamma (..)
) where

import Blucontrol.RGB

class (Monad m, RGB c) => MonadGamma c m | m -> c where

  {- | Calculate an 'RGB' value.
     This is a monadic function, to allow the value to be dependent on side effects like time and
     location.
  -}
  gamma :: m c
