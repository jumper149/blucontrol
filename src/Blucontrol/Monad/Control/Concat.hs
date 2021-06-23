{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Blucontrol.Monad.Control.Concat (
  ControlConcatT
, runControlConcatT
, (!>)
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Kind

import Blucontrol.Monad.Control

newtype ControlConcatT (t1 :: (Type -> Type) -> Type -> Type) (t2 :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a = ControlConcatT { unControlConcatT :: t2 (t1 m) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)

instance (forall m. Monad m => Monad (t1 m), MonadTrans t1, MonadTrans t2) => MonadTrans (ControlConcatT t1 t2) where
  lift = ControlConcatT . lift . lift

instance (forall m. Monad m => Monad (t1 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ControlConcatT t1 t2) where
  type StT (ControlConcatT t1 t2) a = StT t1 (StT t2 a)
  liftWith inner = ControlConcatT $
    liftWith $ \ runT2 ->
      liftWith $ \ runT1 ->
        inner $ runT1 . runT2 . unControlConcatT
  restoreT = ControlConcatT . restoreT . restoreT

instance (MonadControl (t1 m), MonadControl (t2 (t1 m)), MonadTrans t2) => MonadControl (ControlConcatT t1 t2 m) where
  type ControlConstraint (ControlConcatT t1 t2 m) a = (ControlConstraint (t1 m) a, ControlConstraint (t2 (t1 m)) a)
  doInbetween a = do ControlConcatT . lift $ doInbetween a
                     ControlConcatT $ doInbetween a

runControlConcatT :: (forall a. t1 m a -> m (StT t1 a))
                  -> (forall a. t2 (t1 m) a -> t1 m (StT t2 a))
                  -> (forall a. ControlConcatT t1 t2 m a -> m (StT t1 (StT t2 a)))
runControlConcatT runT1 runT2 = runT1 . runT2 . unControlConcatT

infixr 5 !>
(!>) :: (forall a. t1 m a -> m (StT t1 a))
     -> (forall a. t2 (t1 m) a -> t1 m (StT t2 a))
     -> (forall a. ControlConcatT t1 t2 m a -> m (StT t1 (StT t2 a)))
(!>) = runControlConcatT
