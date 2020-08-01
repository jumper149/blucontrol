{-# LANGUAGE UndecidableInstances #-}

module Blumon.Control (
  MonadControl (..)
) where

import Control.Monad.Trans.Control
import Data.Kind

import Blumon.Config

class MonadBaseControl IO m => MonadControl m where
  type ControlConstraint m a :: Constraint
  doInbetween :: ControlConstraint m a => Config -> a -> m ()

instance MonadControl IO where
  type ControlConstraint IO a = ()
  doInbetween _ _ = return ()
