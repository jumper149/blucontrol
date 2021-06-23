{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Monad.Control.Count (
  ControlCountT
, runControlCountT
, ConfigCount (..)
, CountableException (..)
) where

import Control.DeepSeq
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Default
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Default
import GHC.Generics
import Numeric.Natural

import Blucontrol.Monad.Control

newtype ControlCountT m a = ControlCountT { unControlCountT :: StateT Natural (ReaderT ConfigCount m) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)
  deriving (MonadTrans, MonadTransControl) via Stack2T (StateT Natural) (ReaderT ConfigCount)

instance MonadBaseControl IO m => MonadControl (ControlCountT m) where
  type ControlConstraint (ControlCountT m) a = CountableException a
  doInbetween a = do if isException a
                        then ControlCountT $ modify succ
                        else ControlCountT $ put 0
                     current <- ControlCountT get
                     limit <- ControlCountT . lift $ reader maxCount
                     if current >= limit
                        then error $ "failed after " <> show limit <> " consecutive tries"
                        else return ()

runControlCountT :: Monad m => ConfigCount -> ControlCountT m a -> m (a, Natural)
runControlCountT !conf tma = runReaderT (runStateT (unControlCountT tma) 0) conf

newtype ConfigCount = ConfigCount { maxCount :: Natural
                                  }
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData ConfigCount

instance Default ConfigCount where
  def = ConfigCount { maxCount = 5
                    }

class CountableException a where
  isException :: a -> Bool

instance CountableException () where
  isException () = False

instance CountableException a => CountableException (Maybe a) where
  isException Nothing = True
  isException (Just a) = isException a

instance CountableException a => CountableException (Either b a) where
  isException (Left _) = True
  isException (Right a) = isException a
