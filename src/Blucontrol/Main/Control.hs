{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Main.Control (
  ControlT
, runControlT
, loopRecolor
, ConfigControl (..)
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State.Strict

import Blucontrol.Control
import Blucontrol.Gamma
import Blucontrol.Recolor
import Blucontrol.RGB

newtype ControlT c m a = ControlT { unControlT :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b)

instance MonadTrans (ControlT c) where
  lift = ControlT

instance MonadTransControl (ControlT c) where
  type StT (ControlT c) a = a
  liftWith inner = ControlT $ inner unControlT
  restoreT = ControlT

runControlT :: Monad m
            => ControlT c m a
            -> m a
runControlT = unControlT

loopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma c g, MonadRecolor r)
            => (forall a. g a -> IO (StM g a))
            -> (forall a. r a -> g (StM r a))
            -> ControlT c m ()
loopRecolor runG runR = do
  a <- liftBase doRecolorGamma
  ControlT $ evalStateT doLoopRecolor a
  where doRecolorGamma = runG $ do
          rgb <- toRGB <$> gamma
          runR $ recolor rgb
        doLoopRecolor = do
          a' <- get
          lift $ doInbetween a'
          a'' <- liftBase doRecolorGamma
          put a''
          doLoopRecolor

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO a
                                         , runGamma   :: forall a. g a -> IO (StM g a)
                                         , runRecolor :: forall a. r a -> g (StM r a)
                                         }
