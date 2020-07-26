module Blumon.Main (
  blumon
, ConfigControl (..)
) where

import Control.Monad.Trans.Control

import Blumon.Config
import Blumon.Control
import Blumon.Monad.Control
import Blumon.Monad.Recolor

blumon :: (MonadControl m, MonadBaseControl IO r, MonadRecolor r) => Config -> ConfigControl m r -> IO [StM r ()]
blumon c cc = runControl cc . runControlT c . loopRecolor $ runRecolor cc
