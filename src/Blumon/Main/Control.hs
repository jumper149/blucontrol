{-# LANGUAGE UndecidableInstances #-}

module Blumon.Main.Control (
  ControlT
, runControlT
, loopRecolor
, ConfigControl (..)
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S (evalStateT)

import Blumon.Config
import Blumon.Monad.Control
import Blumon.Monad.Recolor

newtype ControlT m a = ControlT { unControlT :: S.SerialT (ReaderT Config m) a }
  deriving (Applicative, Functor, Monad)

instance MonadTrans ControlT where
  lift = ControlT . lift . lift

instance MonadBase b m => MonadBase b (ControlT m) where
  liftBase = liftBaseDefault

runControlT :: Monad m
            => Config
            -> ControlT m a
            -> m [a]
runControlT conf tma = runReaderT (S.toList $ unControlT tma) conf

loopRecolor :: (MonadControl m, MonadBaseControl IO r, MonadRecolor r)
            => (r () -> m (StM r ()))
            -> ControlT m (StM r ())
loopRecolor run = do a' <- lift $ run recolor
                     ControlT . (a' S..:) . S.evalStateT a' $ do
                       S.repeatM $ do a <- get
                                      conf <- ask
                                      lift . lift $ doInbetween conf a
                                      lift . lift $ run recolor

data ConfigControl m r = ConfigControl { runControl :: forall a. m a -> IO a
                                       , runRecolor :: forall a. r a -> m (StM r a)
                                       }
