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

type BlucontrolConstraints mc mp ma =
  ( CompatibleValues (PreparedValue mp) (ApplicableValue ma)
  , ControlConstraint mc (StM mp (StM ma ()))
  , MonadBaseControl IO mp
  , MonadBaseControl IO ma
  , MonadApplyValue ma
  , MonadControl mc
  , MonadPrepareValue mp
  )

blucontrol :: BlucontrolConstraints mc mp ma
           => ConfigControl mc mp ma
           -> IO (StM mc (StM mp (StM ma ())))
blucontrol ConfigControl { runControl, runPrepareValue, runApplyValue } = do
  launch
  runControl $ liftBaseWith $ \ runC ->
    runPrepareValue $ liftBaseWith $ \ runP ->
      runApplyValue $ liftBaseWith $ \ runA ->
        loopRecolor runC runP runA convertValue

data ConfigControl mc mp ma = ConfigControl { runControl :: forall a. mc a -> IO (StM mc a)
                                            , runPrepareValue :: forall a. mp a -> IO (StM mp a)
                                            , runApplyValue :: forall a. ma a -> IO (StM ma a)
                                            }
