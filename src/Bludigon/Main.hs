module Bludigon.Main (
  bludigon
, ConfigControl (..)
) where

import Control.Monad.Trans.Control

import Bludigon.Config
import Bludigon.Main.Control
import Bludigon.Main.CLI
import Bludigon.Control
import Bludigon.Gamma
import Bludigon.Recolor

bludigon :: (ControlConstraint m (StM g (StM r ())), MonadControl m, MonadBaseControl IO g, MonadBaseControl IO r, MonadGamma g, MonadRecolor r)
         => Config
         -> ConfigControl m g r
         -> IO ()
bludigon c cc = do launch
                   runControl cc . runControlT c $ loopRecolor (runGamma cc) (runRecolor cc)
