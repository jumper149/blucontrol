module Blucontrol.Main.Control (
  loopRecolor
) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Unsafe.Coerce

import Blucontrol.Monad.ApplyValue
import Blucontrol.Monad.Control
import Blucontrol.Monad.PrepareValue

-- | Run the loop, using `prepareValue`, `applyValue` and `doInbetween`.
loopRecolor :: (ControlConstraint mc (StM mp (StM ma ())), MonadBaseControl IO mc, MonadBaseControl IO mp, MonadBaseControl IO ma, MonadApplyValue ma, MonadControl mc, MonadPrepareValue mp)
            => RunInBase mc IO
            -> RunInBase mp IO
            -> RunInBase ma IO
            -> (PreparedValue mp -> ApplicableValue ma)
            -> IO ()
loopRecolor runC runP runA coerceValue = do

      -- Use `preparedValue` and give the result to `applyValue`.
      -- Then use the result of `applyValue` and give it to `doInbetween` including the monadic state.
      -- The argument is an initial monadic state.
  let doRecolor x =
        runC $ do
          x1 <- restoreM x
          x4 <- liftBase $ do
            runP $ do
              x2 <- restoreM x1
              value <- coerceValue <$> preparedValue
              liftBase $ runA $ do
                x3 <- restoreM x2
                applyValue value
                pure x3
          let currentRecolorValue =
                runP $ do
                  -- TODO: `unsafeCoerce` is necessary because of GHC limitations with type families.
                  -- `unsafeCoerce` will act like `id`.
                  x5 <- restoreM $ unsafeCoerce x4
                  liftBase $ runA $ void $ restoreM x5
          currentRecolorValue' <- liftBase currentRecolorValue
          doInbetween currentRecolorValue'
          pure x4

      -- Run `doLoopRecolor` in a recursive loop while passing the monadic state explicitly.
      doLoopRecolor x = doLoopRecolor =<< liftBase (doRecolor x)

  -- Initialize the monadic state.
  initStM <- runC $ liftBase $ runP $ liftBase $ runA $ pure undefined

  -- Start an infinite loop.
  -- TODO: `unsafeCoerce` is necessary because of GHC limitations with type families.
  -- `unsafeCoerce` will act like `id`.
  doLoopRecolor $ unsafeCoerce initStM
