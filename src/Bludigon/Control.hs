{-# LANGUAGE UndecidableInstances #-}

module Bludigon.Control (
  MonadControl (..)
) where

import Control.Monad.Trans.Control
import Data.Kind

class MonadBaseControl IO m => MonadControl m where
  type ControlConstraint m a :: Constraint
  doInbetween :: ControlConstraint m a => a -> m ()

instance MonadControl IO where
  type ControlConstraint IO a = ()
  doInbetween _ = return ()
