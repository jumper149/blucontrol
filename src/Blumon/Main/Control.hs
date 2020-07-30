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
import Blumon.Control
import Blumon.Gamma
import Blumon.Recolor

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

loopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma g, MonadRecolor r)
            => (forall a. g a -> m (StM g a))
            -> (forall a. r a -> g (StM r a))
            -> ControlT m (StM g (StM r ()))
loopRecolor runG runR = do
  a <- lift $ doRecolorGamma
  ControlT . (a S..:) . S.evalStateT a $ do
    S.repeatM $ do
      a' <- get
      conf <- ask
      lift . lift $ doInbetween conf a'
      a'' <- lift $ lift $ doRecolorGamma
      put a''
      return a''
  where doRecolorGamma = runG $ do
          rgb <- gamma
          runR $ recolor rgb

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO a
                                         , runGamma   :: forall a. g a -> m (StM g a)
                                         , runRecolor :: forall a. r a -> g (StM r a)
                                         }
