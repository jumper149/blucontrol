{-# LANGUAGE UndecidableInstances #-}

module Blumon.Control (
  MonadControl (..)
) where

import Data.Kind
import qualified Streamly as S

import Blumon.Config

class S.MonadAsync m => MonadControl m where
  type ControlConstraint m a :: Constraint
  doInbetween :: ControlConstraint m a => Config -> a -> m ()

instance MonadControl IO where
  type ControlConstraint IO a = ()
  doInbetween _ _ = return ()
