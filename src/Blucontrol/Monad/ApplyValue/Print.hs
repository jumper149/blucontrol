module Blucontrol.Monad.ApplyValue.Print (
  ApplyValuePrintT
, runApplyValuePrintT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Default

import Blucontrol.Monad.ApplyValue

newtype ApplyValuePrintT c m a = ApplyValuePrintT { unApplyValuePrintT :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)
  deriving (MonadTrans, MonadTransControl) via Stack0T

instance (MonadBaseControl IO m, Show c) => MonadApplyValue (ApplyValuePrintT c m) where
  type ApplicableValue (ApplyValuePrintT c m) = c
  applyValue = liftBase . print

runApplyValuePrintT :: ApplyValuePrintT c m a -> m a
runApplyValuePrintT = unApplyValuePrintT
