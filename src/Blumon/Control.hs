module Blumon.Control (
  loopRecolor
) where

import Control.Concurrent
import Control.Monad.Base

import Blumon.Config
import Blumon.Monad.Recolor

loopRecolor :: (MonadBase IO m, MonadRecolor m) => Config -> (forall a. m a -> IO a) -> IO ()
loopRecolor conf run = do
  run recolor
  putStrLn "1"
  threadDelay $ interval conf
  putStrLn "2"
  loopRecolor conf run
