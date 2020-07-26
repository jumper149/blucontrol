module Blumon.Main (
  blumon
, ConfigControl (..)
) where

import Control.Monad (void)
import Control.Monad.Trans.Control

import Blumon.Config
import Blumon.Main.Control
import Blumon.Monad.Control
import Blumon.Monad.Recolor

blumon :: (MonadControl m, MonadBaseControl IO r, MonadRecolor r) => Config -> ConfigControl m r -> IO ()
blumon c cc = void . runControl cc . runControlT c . loopRecolor $ runRecolor cc
