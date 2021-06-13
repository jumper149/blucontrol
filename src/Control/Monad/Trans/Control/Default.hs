{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Control.Monad.Trans.Control.Default (
  Stack2T (..)
) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Kind

newtype Stack2T
  (t1 :: (Type -> Type) -> Type -> Type)
  (t2 :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
    = CombinerT { unCombinerT :: t1 (t2 m) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)

instance ((forall m. Monad m => Monad (t2 m)), MonadTrans t1, MonadTrans t2) => MonadTrans (Stack2T t1 t2) where
    lift = CombinerT . lift . lift

instance ((forall m. Monad m => Monad (t2 m)), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (Stack2T t1 t2) where
  type StT (Stack2T t1 t2) a = StT t2 (StT t1 a)
  liftWith f = CombinerT $ liftWith $ \run -> liftWith $ \run' -> f $ run' . run . unCombinerT
  restoreT = defaultRestoreT2 CombinerT
