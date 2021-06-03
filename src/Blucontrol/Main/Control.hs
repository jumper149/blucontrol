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

-- | Run the loop, using `gamma`, `recolor` and `doInbetween`.
-- The arguments are the actual monad runners.
loopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma g, MonadRecolor r)
            => (forall a. g a -> IO (StM g a))
            -> (forall a. r a -> IO (StM r a))
            -> (GammaRGB g -> RecolorRGB r)
            -> m ()
loopRecolor runG runR coerceRGB' = void $
  liftBaseWith $ \ runCIO ->
    runR $ liftBaseWith $ \ runRIO ->
      runG $ liftBaseWith $ \ runGIO -> do
        firstResult <- doRecolorGamma runGIO runRIO coerceRGB'
        evalStateT (doLoopRecolor runCIO runGIO runRIO coerceRGB') firstResult

-- | Use `gamma` and give the result to `recolor`.
-- The arguments are runners from `liftBaseWith`.
doRecolorGamma :: (MonadBaseControl IO g, MonadBaseControl IO r, MonadGamma g, MonadRecolor r)
               => (forall a. g a -> IO (StM g a))
               -> (forall a. r a -> IO (StM r a))
               -> (GammaRGB g -> RecolorRGB r)
               -> IO (StM g (StM r ()))
doRecolorGamma runGIO runRIO coerceRGB' = runGIO $ do
  rgb <- coerceRGB' <$> gamma
  liftBase $ runRIO $ recolor rgb

-- | A single iteration of `loopRecolor`.
-- The arguments are runners from `liftBaseWith`.
doLoopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma g, MonadRecolor r)
              => (forall a. m a -> IO (StM m a))
              -> (forall a. g a -> IO (StM g a))
              -> (forall a. r a -> IO (StM r a))
              -> (GammaRGB g -> RecolorRGB r)
              -> StateT (StM g (StM r ())) IO ()
doLoopRecolor runCIO runGIO runRIO coerceRGB' = do
  lastResult <- get
  void $ liftBase $ runCIO $ doInbetween lastResult
  nextResult <- liftBase $ doRecolorGamma runGIO runRIO coerceRGB'
  put nextResult
  doLoopRecolor runCIO runGIO runRIO coerceRGB'

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO a
                                         , runGamma   :: forall a. g a -> IO (StM g a)
                                         , runRecolor :: forall a. r a -> IO (StM r a)
                                         , coerceRGB  :: GammaRGB g -> RecolorRGB r
                                         }
