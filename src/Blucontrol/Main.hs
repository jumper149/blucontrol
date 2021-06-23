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
           -> IO (StM m (StM g (StM r ())))
blucontrol c = do launch
                  loopRecolor (runControl c) (runGamma c) (runRecolor c) convertValue

data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO (StM m a)
                                         , runGamma   :: forall a. g a -> IO (StM g a)
                                         , runRecolor :: forall a. r a -> IO (StM r a)
                                         }
