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
            => RunInBase m IO
            -> RunInBase g IO
            -> RunInBase r IO
            -> (GammaValue g -> RecolorValue r)
            -> IO ()
loopRecolor runC runG runR coerceValue = do

      -- Use `gamma` and give the result to `recolor`.
      -- Then use the result of `recolor` and give it to `doInbetween` including the monadic state.
      -- The argument is an initial monadic state.
  let doRecolorGamma x =
        runC $ do
          x1 <- restoreM x
          x4 <- liftBase $ do
            runG $ do
              x2 <- restoreM x1
              value <- coerceValue <$> gamma
              liftBase $ runR $ do
                x3 <- restoreM x2
                recolor value
                pure x3
          let currentRecolorValue =
                runG $ do
                  -- TODO: `unsafeCoerce` is necessary because of GHC limitations with type families.
                  -- `unsafeCoerce` will act like `id`.
                  x5 <- restoreM $ unsafeCoerce x4
                  liftBase $ runR $ void $ restoreM x5
          currentRecolorValue' <- liftBase currentRecolorValue
          doInbetween currentRecolorValue'
          pure x4

      -- Run `doLoopRecolor` in a recursive loop while passing the monadic state explicitly.
      doLoopRecolor x = doLoopRecolor =<< liftBase (doRecolorGamma x)

  -- Initialize the monadic state.
  initStM <- runC $ liftBase $ runG $ liftBase $ runR $ pure undefined

  -- Start an infinite loop.
  -- TODO: `unsafeCoerce` is necessary because of GHC limitations with type families.
  -- `unsafeCoerce` will act like `id`.
  doLoopRecolor $ unsafeCoerce initStM
