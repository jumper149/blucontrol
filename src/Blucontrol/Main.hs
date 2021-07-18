module Blucontrol.Main (
  blucontrol
, BlucontrolConstraints
, ConfigControl (..)
) where

import Control.Monad.Trans.Control

import Blucontrol.Main.Control
import Blucontrol.Main.CLI
import Blucontrol.Monad.ApplyValue
import Blucontrol.Monad.Control
import Blucontrol.Monad.PrepareValue
import Blucontrol.Value

type BlucontrolConstraints m g r =
  ( CompatibleValues (PreparedValue g) (ApplicableValue r)
  , ControlConstraint m (StM g (StM r ()))
  , MonadBaseControl IO g
  , MonadBaseControl IO r
  , MonadApplyValue r
  , MonadControl m
  , MonadPrepareValue g
  )

blucontrol :: BlucontrolConstraints m g r
           => ConfigControl m g r
           -> IO (StM m (StM g (StM r ())))
blucontrol ConfigControl { runControl, runPrepareValue, runApplyValue } = do
  launch
  runControl $ liftBaseWith $ \ runC ->
    runPrepareValue $ liftBaseWith $ \ runG ->
      runApplyValue $ liftBaseWith $ \ runR ->
        loopRecolor runC runG runR convertValue

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO (StM m a)
                                         , runPrepareValue :: forall a. g a -> IO (StM g a)
                                         , runApplyValue :: forall a. r a -> IO (StM r a)
                                         }
