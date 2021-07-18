{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Monad.PrepareValue.Modifier (
  PrepareValueModifierT
, runPrepareValueModifierT
) where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Blucontrol.Monad.PrepareValue

newtype PrepareValueModifierT m a = PrepareValueModifierT { unPrepareValueModifierT :: ReaderT (PreparedValue m -> IO (PreparedValue m)) m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)
-- TODO: A `MonadTransControl` instance seems to be impossible

instance MonadTrans PrepareValueModifierT where
  lift = PrepareValueModifierT . lift

instance (MonadBase IO m, MonadPrepareValue m) => MonadPrepareValue (PrepareValueModifierT m) where
  type PreparedValue (PrepareValueModifierT m) = PreparedValue m
  preparedValue = do
    oldValue <- lift preparedValue
    modifyValue <- PrepareValueModifierT ask
    liftBase $ modifyValue oldValue

runPrepareValueModifierT :: (PreparedValue m -> IO (PreparedValue m)) -> PrepareValueModifierT m a -> m a
runPrepareValueModifierT modify tma = runReaderT (unPrepareValueModifierT tma) modify
