module Bludigon.Main (
  bludigon
, ConfigControl (..)
) where

import Control.Monad.Trans.Control

import Bludigon.Main.Control
import Bludigon.Main.CLI
import Bludigon.Control
import Bludigon.Gamma
import Bludigon.Recolor

bludigon :: (ControlConstraint m (StM g (StM r ())), MonadControl m, MonadBaseControl IO g, MonadBaseControl IO r, MonadGamma g, MonadRecolor r)
         => ConfigControl m g r
         -> IO ()
bludigon c = do launch
                runControl c . runControlT $ loopRecolor (runGamma c) (runRecolor c)
