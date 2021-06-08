module Blucontrol.Main.Control (
  loopRecolor
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State.Strict

import Blucontrol.Monad.Control
import Blucontrol.Monad.Gamma
import Blucontrol.Monad.Recolor

-- | Run the loop, using `gamma`, `recolor` and `doInbetween`.
-- The arguments are the actual monad runners.
loopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma g, MonadRecolor r)
            => (forall a. g a -> IO (StM g a))
            -> (forall a. r a -> IO (StM r a))
            -> (GammaValue g -> RecolorValue r)
            -> m ()
loopRecolor runG runR coerceValue = void $
  liftBaseWith $ \ runCIO ->
    runR $ liftBaseWith $ \ runRIO ->
      runG $ liftBaseWith $ \ runGIO -> do
        firstResult <- doRecolorGamma runGIO runRIO coerceValue
        evalStateT (doLoopRecolor runCIO runGIO runRIO coerceValue) firstResult

-- | Use `gamma` and give the result to `recolor`.
-- The arguments are runners from `liftBaseWith`.
doRecolorGamma :: (MonadBaseControl IO g, MonadBaseControl IO r, MonadGamma g, MonadRecolor r)
               => (forall a. g a -> IO (StM g a))
               -> (forall a. r a -> IO (StM r a))
               -> (GammaValue g -> RecolorValue r)
               -> IO (StM g (StM r ()))
doRecolorGamma runGIO runRIO coerceValue = runGIO $ do
  value <- coerceValue <$> gamma
  liftBase $ runRIO $ recolor value

-- | A single iteration of `loopRecolor`.
-- The arguments are runners from `liftBaseWith`.
doLoopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma g, MonadRecolor r)
              => (forall a. m a -> IO (StM m a))
              -> (forall a. g a -> IO (StM g a))
              -> (forall a. r a -> IO (StM r a))
              -> (GammaValue g -> RecolorValue r)
              -> StateT (StM g (StM r ())) IO ()
doLoopRecolor runCIO runGIO runRIO coerceValue = do
  lastResult <- get
  void $ liftBase $ runCIO $ doInbetween lastResult
  nextResult <- liftBase $ doRecolorGamma runGIO runRIO coerceValue
  put nextResult
  doLoopRecolor runCIO runGIO runRIO coerceValue
