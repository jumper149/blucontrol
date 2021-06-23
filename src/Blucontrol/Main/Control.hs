module Blucontrol.Main.Control (
  loopRecolor
) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Unsafe.Coerce

import Blucontrol.Monad.Control
import Blucontrol.Monad.Gamma
import Blucontrol.Monad.Recolor

-- | Run the loop, using `gamma`, `recolor` and `doInbetween`.
loopRecolor :: (MonadBaseControl IO m, MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma g, MonadRecolor r, ControlConstraint m (StM g (StM r ())))
            => (forall a. m a -> IO (StM m a))
            -> (forall a. g a -> IO (StM g a))
            -> (forall a. r a -> IO (StM r a))
            -> (GammaValue g -> RecolorValue r)
            -> IO (StM m (StM g (StM r ())))
loopRecolor runC runG runR coerceValue = do
  runC $ liftBaseWith $ \ runCIO ->
    runG $ liftBaseWith $ \ runGIO ->
      runR $ liftBaseWith $ \ runRIO -> do

            -- Use `gamma` and give the result to `recolor`.
            -- Then use the result of `recolor` and give it to `doInbetween` including the monadic state.
            -- The argument is an initial monadic state.
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

            -- Run `doLoopRecolor` in a recursive loop while passing the monadic state explicitly.
            doLoopRecolor x = doLoopRecolor =<< liftBase (doRecolorGamma x)

        -- Initialize the monadic state.
        initStM <- runCIO $ liftBase $ runGIO $ liftBase $ runRIO $ pure undefined

        -- Start an infinite loop.
        void $ doLoopRecolor $ unsafeCoerce initStM
