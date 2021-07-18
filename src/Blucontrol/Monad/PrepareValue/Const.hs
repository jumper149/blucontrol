{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Monad.PrepareValue.Const (
  PrepareValueConstT
, runPrepareValueConstT
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Reader

import Blucontrol.Monad.PrepareValue

newtype PrepareValueConstT c m a = PrepareValueConstT { unPrepareValueConstT :: ReaderT c m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance Monad m => MonadPrepareValue (PrepareValueConstT c m) where
  type PreparedValue (PrepareValueConstT c m) = c
  preparedValue = PrepareValueConstT ask

runPrepareValueConstT :: c -> PrepareValueConstT c m a -> m a
runPrepareValueConstT !rgb tma = runReaderT (unPrepareValueConstT tma) rgb
