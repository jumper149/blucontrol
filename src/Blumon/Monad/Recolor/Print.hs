{-# LANGUAGE UndecidableInstances #-}

module Blumon.Monad.Recolor.Print (
  RecolorPrintT
, runRecolorPrintT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control

import Blumon.Monad.Gamma
import Blumon.Monad.Recolor

newtype RecolorPrintT m a = RecolorPrintT { unRecolorPrintT :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadGamma)

instance MonadTrans RecolorPrintT where
  lift = RecolorPrintT

instance MonadTransControl RecolorPrintT where
  type StT RecolorPrintT a = a
  liftWith inner = RecolorPrintT $ inner unRecolorPrintT
  restoreT = RecolorPrintT

instance (MonadBaseControl IO m, MonadGamma m) => MonadRecolor (RecolorPrintT m) where
  recolor = liftBase . print =<< gamma

runRecolorPrintT :: RecolorPrintT m a -> m a
runRecolorPrintT = unRecolorPrintT
