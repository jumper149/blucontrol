module Blucontrol.Main.Control (
  loopRecolor
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Unsafe.Coerce

import Blucontrol.Monad.Control
import Blucontrol.Monad.Gamma
import Blucontrol.Monad.Recolor

-- | Run the loop, using `gamma`, `recolor` and `doInbetween`.
loopRecolor :: (MonadBaseControl IO m, MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma g, MonadRecolor r, ControlConstraint m (StM g (StM r ())))
            => (forall a. m a -> IO a)
            -> (forall a. g a -> IO (StM g a))
            -> (forall a. r a -> IO (StM r a))
            -> (GammaValue g -> RecolorValue r)
            -> IO ()
loopRecolor runC runG runR coerceValue = void $ do
  runC $ liftBaseWith $ \ runCIO ->
    runR $ liftBaseWith $ \ runRIO ->
      runG $ liftBaseWith $ \ runGIO -> do

            -- Use `gamma` and give the result to `recolor`.
            -- The arguments are runners from `liftBaseWith`.
        let doRecolorGamma x =
              runCIO $ do
                x1 <- restoreM x
                x4 <- liftBase $ do
                  runGIO $ do
                    x2 <- restoreM x1
                    value <- coerceValue <$> gamma
                    liftBase $ runRIO $ do
                      x3 <- restoreM x2
                      recolor value
                      pure x3
                let currentRecolorValue =
                      runGIO $ do
                          x5 <- restoreM $ unsafeCoerce x4
                          liftBase $ runRIO $ do
                            void $ restoreM x5
                            pure ()
                currentRecolorValue' <- liftBase currentRecolorValue
                doInbetween currentRecolorValue'
                pure x4

            doLoopRecolor x = do
              x' <- liftBase $ doRecolorGamma x
              doLoopRecolor x'

        initStM <- runCIO $ liftBase $ runGIO $ liftBase $ runRIO $ pure undefined
        void $ doLoopRecolor $ unsafeCoerce initStM
        pure ()
