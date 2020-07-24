module Blumon.Control (
  loopRecolor
) where

import Control.Concurrent
import qualified Data.Text as T

import Blumon.Config
import Blumon.Monad.Recolor

loopRecolor :: MonadRecolor m => Config -> (forall a. m a -> IO (Either T.Text a)) -> IO ()
loopRecolor conf run = do
  recolorSuccess <- run recolor
  case recolorSuccess of
    Right _ -> return ()
    Left t -> putStrLn $ T.unpack t
  putStrLn "1"
  threadDelay $ interval conf
  putStrLn "2"
  loopRecolor conf run
