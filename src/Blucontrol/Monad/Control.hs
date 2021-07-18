{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Monad.Control (
  MonadControl (..)
) where

import Control.Monad.Trans.Control
import Data.Kind

class MonadBaseControl IO m => MonadControl m where

  {- | Give a constraint to allow 'doInbetween' to handle a polymorphic argument.
    This is usfeul to allow arguments wrapped in monadic state 'StM' from running
    'Blucontrol.Monad.PrepareValue' and 'Blucontrol.Monad.ApplyValue'.
  -}
  type ControlConstraint m a :: Constraint

  -- | This function will be called after recoloring the screen.
  doInbetween :: ControlConstraint m a
              => a -- ^ the returned value from the last call of 'Blucontrol.Monad.ApplyValue.applyValue' including monadic state
              -> m () -- ^ the side effect to be run inbetween recoloring

instance MonadControl IO where
  type ControlConstraint IO _ = ()
  doInbetween _ = return ()
