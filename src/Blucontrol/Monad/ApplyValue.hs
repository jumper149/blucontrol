module Blucontrol.Monad.ApplyValue (
  MonadApplyValue (..)
) where

class Monad m => MonadApplyValue m where

  type ApplicableValue m

  {- | Apply a value.
     This is a monadic function, to allow application to external programs like an X display
     server.
  -}
  applyValue :: ApplicableValue m -> m ()
