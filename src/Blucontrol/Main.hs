module Blucontrol.Main (
  blucontrol
, BlucontrolConstraints
, ConfigControl (..)
) where

import Control.Monad.Trans.Control

import Blucontrol.Main.Control
import Blucontrol.Main.CLI
import Blucontrol.Monad.Control
import Blucontrol.Monad.Gamma
import Blucontrol.Monad.Recolor
import Blucontrol.Value

type BlucontrolConstraints m g r =
  ( CompatibleValues (GammaValue g) (RecolorValue r)
  , ControlConstraint m (StM g (StM r ()))
  , MonadBaseControl IO g
  , MonadBaseControl IO r
  , MonadControl m
  , MonadGamma g
  , MonadRecolor r
  )

blucontrol :: BlucontrolConstraints m g r
           => ConfigControl m g r
           -> IO (StM m (StM g (StM r ())))
blucontrol ConfigControl { runControl, runGamma, runRecolor } = do
  launch
  runControl $ liftBaseWith $ \ runC ->
    runGamma $ liftBaseWith $ \ runG ->
      runRecolor $ liftBaseWith $ \ runR ->
        loopRecolor runC runG runR convertValue

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO (StM m a)
                                         , runGamma   :: forall a. g a -> IO (StM g a)
                                         , runRecolor :: forall a. r a -> IO (StM r a)
                                         }
