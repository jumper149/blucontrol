{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Monad.Control.Print (
  ControlPrintT
, runControlPrintT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Default

import Blucontrol.Monad.Control

newtype ControlPrintT m a = ControlPrintT { unControlPrintT :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)
  deriving (MonadTrans, MonadTransControl) via Stack0T

instance MonadBaseControl IO m => MonadControl (ControlPrintT m) where
  type ControlConstraint (ControlPrintT m) a = Show a
  doInbetween a = liftBase $ print a

runControlPrintT :: ControlPrintT m a -> m a
runControlPrintT = unControlPrintT
