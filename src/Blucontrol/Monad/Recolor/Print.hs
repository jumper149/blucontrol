module Blucontrol.Monad.Recolor.Print (
  RecolorPrintT
, runRecolorPrintT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Default

import Blucontrol.Monad.Recolor

newtype RecolorPrintT c m a = RecolorPrintT { unRecolorPrintT :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)
  deriving (MonadTrans, MonadTransControl) via Stack0T

instance (MonadBaseControl IO m, Show c) => MonadRecolor (RecolorPrintT c m) where
  type RecolorValue (RecolorPrintT c m) = c
  recolor = liftBase . print

runRecolorPrintT :: RecolorPrintT c m a -> m a
runRecolorPrintT = unRecolorPrintT
