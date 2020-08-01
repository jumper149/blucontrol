{-# LANGUAGE UndecidableInstances #-}

module Bludigon.Main.Control (
  ControlT
, runControlT
, loopRecolor
, ConfigControl (..)
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State.Strict

import Bludigon.Config
import Bludigon.Control
import Bludigon.Gamma
import Bludigon.Recolor

newtype ControlT m a = ControlT { unControlT :: ReaderT Config m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadTrans, MonadTransControl)

runControlT :: Monad m
            => Config
            -> ControlT m a
            -> m a
runControlT conf tma = runReaderT (unControlT tma) conf

loopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma g, MonadRecolor r)
            => (forall a. g a -> m (StM g a))
            -> (forall a. r a -> g (StM r a))
            -> ControlT m ()
loopRecolor runG runR = do
  a <- lift $ doRecolorGamma
  ControlT $ evalStateT doLoopRecolor a
  where doRecolorGamma = runG $ do
          rgb <- gamma
          runR $ recolor rgb
        doLoopRecolor = do
          a' <- get
          conf <- ask
          lift . lift $ doInbetween conf a'
          a'' <- lift $ lift $ doRecolorGamma
          put a''
          doLoopRecolor

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO a
                                         , runGamma   :: forall a. g a -> m (StM g a)
                                         , runRecolor :: forall a. r a -> g (StM r a)
                                         }
