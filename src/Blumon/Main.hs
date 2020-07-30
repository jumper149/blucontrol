module Blumon.Main (
  blumon
, ConfigControl (..)
) where

import Control.Monad (void)
import Control.Monad.Trans.Control

import Blumon.Config
import Blumon.Main.Control
import Blumon.Main.CLI
import Blumon.Control
import Blumon.Gamma
import Blumon.Recolor

blumon :: (ControlConstraint m (StM g (StM r ())), MonadControl m, MonadBaseControl IO g,MonadBaseControl IO r, MonadGamma g, MonadRecolor r)
       => Config
       -> ConfigControl m g r
       -> IO ()
blumon c cc = do launch
                 void . runControl cc . runControlT c $ loopRecolor (runGamma cc) (runRecolor cc)
