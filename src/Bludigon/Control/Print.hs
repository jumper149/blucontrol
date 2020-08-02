{-# LANGUAGE UndecidableInstances #-}

module Bludigon.Control.Print (
  ControlPrintT
, runControlPrintT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control

import Bludigon.Control

newtype ControlPrintT m a = ControlPrintT { unControlPrintT :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)

instance MonadTrans ControlPrintT where
  lift = ControlPrintT

instance MonadTransControl ControlPrintT where
  type StT ControlPrintT a = a
  liftWith inner = ControlPrintT $ inner unControlPrintT
  restoreT = ControlPrintT

instance MonadControl m => MonadControl (ControlPrintT m) where
  type ControlConstraint (ControlPrintT m) a = (ControlConstraint m a, Show a)
  doInbetween a = do liftBase $ print a
                     lift $ doInbetween a

runControlPrintT :: ControlPrintT m a -> m a
runControlPrintT = unControlPrintT
