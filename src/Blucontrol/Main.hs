module Blucontrol.Main (
  blucontrol
, BlucontrolConstraints
, ConfigControl (..)
) where

import Control.Monad.Trans.Control

import Blucontrol.CompatibleValues
import Blucontrol.Main.Control
import Blucontrol.Main.CLI
import Blucontrol.Control
import Blucontrol.Gamma
import Blucontrol.Recolor

type BlucontrolConstraints m g r =
  ( ControlConstraint m (StM g (StM r ()))
  , MonadControl m
  , MonadBaseControl IO g
  , MonadBaseControl IO r
  , MonadGamma g
  , MonadRecolor r
  , CompatibleValues (GammaValue g) (RecolorValue r)
  )

blucontrol :: BlucontrolConstraints m g r
           => ConfigControl m g r
           -> IO ()
blucontrol c = do launch
                  runControl c $ loopRecolor (runGamma c) (runRecolor c) convertValue

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO a
                                         , runGamma   :: forall a. g a -> IO (StM g a)
                                         , runRecolor :: forall a. r a -> IO (StM r a)
                                         }
