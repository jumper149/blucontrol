{-# LANGUAGE UndecidableInstances #-}

module Blumon.Control (
  ControlT
, runControlT
, loopRecolor
) where

import Control.Monad.Base
import Control.Monad.List
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S (evalStateT)

import Blumon.Config
import Blumon.Monad.Recolor

newtype ControlT m a = ControlT { unControlT :: S.SerialT (ReaderT Config m) a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans ControlT where
  lift = ControlT . lift . lift

instance MonadBase b m => MonadBase b (ControlT m) where
  liftBase = liftBaseDefault

class S.MonadAsync m => MonadLoopControl m where
  doInbetween :: a -> m ()

runControlT :: Monad m
            => Config
            -> ControlT m a
            -> m [a]
runControlT conf tma = runReaderT (S.toList $ unControlT tma) conf

loopRecolor :: (MonadLoopControl m, MonadBaseControl IO r, MonadRecolor r)
            => (r () -> m (StM r ()))
            -> ControlT m (StM r ())
loopRecolor run = do a' <- lift $ run recolor
                     ControlT $ (a' S..:) $ S.evalStateT a' $ do
                       S.repeatM $ do a <- get
                                      lift . lift $ doInbetween a
                                      lift . lift $ run recolor
