module Blucontrol.Main.Control (
  loopRecolor
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

-- | Run the loop, using `gamma`, `recolor` and `doInbetween`.
-- The arguments are the actual monad runners.
loopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma c g, MonadRecolor r)
            => (forall a. g a -> IO (StM g a))
            -> (forall a. r a -> IO (StM r a))
            -> m ()
loopRecolor runG runR = void $
  liftBaseWith $ \ runCIO ->
    runR $ liftBaseWith $ \ runRIO ->
      runG $ liftBaseWith $ \ runGIO -> do
        firstResult <- doRecolorGamma runGIO runRIO
        evalStateT (doLoopRecolor runCIO runGIO runRIO) firstResult

-- | Use `gamma` and give the result to `recolor`.
-- The arguments are runners from `liftBaseWith`.
doRecolorGamma :: (MonadBaseControl IO g, MonadBaseControl IO r, MonadGamma c g, MonadRecolor r)
               => (forall a. g a -> IO (StM g a))
               -> (forall a. r a -> IO (StM r a))
               -> IO (StM g (StM r ()))
doRecolorGamma runGIO runRIO = runGIO $ do
  rgb <- toRGB <$> gamma
  liftBase $ runRIO $ recolor rgb

-- | A single iteration of `loopRecolor`.
-- The arguments are runners from `liftBaseWith`.
doLoopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma c g, MonadRecolor r)
              => (forall a. m a -> IO (StM m a))
              -> (forall a. g a -> IO (StM g a))
              -> (forall a. r a -> IO (StM r a))
              -> StateT (StM g (StM r ())) IO ()
doLoopRecolor runCIO runGIO runRIO = do
  lastResult <- get
  void $ liftBase $ runCIO $ doInbetween lastResult
  nextResult <- liftBase $ doRecolorGamma runGIO runRIO
  put nextResult
  doLoopRecolor runCIO runGIO runRIO

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO a
                                         , runGamma   :: forall a. g a -> IO (StM g a)
                                         , runRecolor :: forall a. r a -> IO (StM r a)
                                         }
