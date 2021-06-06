{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Recolor.Print (
  RecolorPrintT
, runRecolorPrintT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control

import Blucontrol.Recolor

newtype RecolorPrintT c m a = RecolorPrintT { unRecolorPrintT :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)

instance MonadTrans (RecolorPrintT c) where
  lift = RecolorPrintT

instance MonadTransControl (RecolorPrintT c) where
  type StT (RecolorPrintT c) a = a
  liftWith inner = RecolorPrintT $ inner unRecolorPrintT
  restoreT = RecolorPrintT

instance (MonadBaseControl IO m, Show c) => MonadRecolor (RecolorPrintT c m) where
  type RecolorRGB (RecolorPrintT c m) = c
  recolor = liftBase . print

runRecolorPrintT :: RecolorPrintT c m a -> m a
runRecolorPrintT = unRecolorPrintT
