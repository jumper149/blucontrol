module Blucontrol.Main (
  blucontrol
, ConfigControl (..)
) where

import Control.Monad.Trans.Control

import Blucontrol.Main.Control
import Blucontrol.Main.CLI
import Blucontrol.Control
import Blucontrol.Gamma
import Blucontrol.Recolor

blucontrol :: (ControlConstraint m (StM g (StM r ())), MonadControl m, MonadBaseControl IO g, MonadBaseControl IO r, MonadGamma c g, MonadRecolor r)
           => ConfigControl m g r
           -> IO ()
blucontrol c = do launch
                  runControl c . runControlT $ loopRecolor (runGamma c) (runRecolor c)
