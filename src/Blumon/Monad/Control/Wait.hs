{-# LANGUAGE UndecidableInstances #-}

module Blumon.Monad.Control.Wait (
  ControlWaitT
, runControlWaitT
, ConfigWait (..)
, Microseconds
) where

import Control.Concurrent (threadDelay)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Data.Default
import GHC.Generics
import qualified Streamly as S

import Blumon.Monad.Control

newtype ControlWaitT m a = ControlWaitT { unControlWaitT :: ReaderT ConfigWait m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadIO, MonadThrow, MonadTrans, MonadTransControl)

instance S.MonadAsync m => MonadControl (ControlWaitT m) where
  doInbetween _ _ = do liftBase . threadDelay . interval =<< ControlWaitT ask

runControlWaitT :: ConfigWait -> ControlWaitT m a -> m a
runControlWaitT conf tma = runReaderT (unControlWaitT tma) conf

data ConfigWait = ConfigWait { interval :: Microseconds
                             }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default ConfigWait where
  def = ConfigWait { interval = 1000000
                   }

type Microseconds = Int
