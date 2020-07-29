module Blumon.Main (
  blumon
, ConfigControl (..)
) where

import Control.Monad (void)
import Control.Monad.Trans.Control

import Blumon.Config
import Blumon.Main.Control
import Blumon.Control
import Blumon.Recolor

blumon :: (ControlConstraint m (StM r ()), MonadControl m, MonadBaseControl IO r, MonadRecolor r) => Config -> ConfigControl m r -> IO ()
blumon c cc = void . runControl cc . runControlT c . loopRecolor $ runRecolor cc
