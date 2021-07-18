module Blucontrol.Monad.PrepareValue (
  MonadPrepareValue (..)
) where

class Monad m => MonadPrepareValue m where

  type PreparedValue m

  {- | Calculate a value.
     This is a monadic function, to allow the value to be dependent on side effects like time and
     location.
  -}
  preparedValue :: m (PreparedValue m)
