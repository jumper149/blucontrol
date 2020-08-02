{-# LANGUAGE UndecidableInstances #-}

module Bludigon.Control.Wait (
  ControlWaitT
, runControlWaitT
, ConfigWait (..)
, Microseconds
) where

import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Data.Default
import GHC.Generics

import Bludigon.Control

newtype ControlWaitT m a = ControlWaitT { unControlWaitT :: ReaderT ConfigWait m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

instance MonadControl m => MonadControl (ControlWaitT m) where
  type ControlConstraint (ControlWaitT m) a = ControlConstraint m a
  doInbetween a = do liftBase . threadDelay . interval =<< ControlWaitT ask
                     lift $ doInbetween a

runControlWaitT :: ConfigWait -> ControlWaitT m a -> m a
runControlWaitT conf tma = runReaderT (unControlWaitT tma) conf

data ConfigWait = ConfigWait { interval :: Microseconds
                             }
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData ConfigWait

instance Default ConfigWait where
  def = ConfigWait { interval = 1000000
                   }

type Microseconds = Int
