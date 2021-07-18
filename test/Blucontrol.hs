module Main where

import Test.Hspec

import qualified Blucontrol.Monad.PrepareValue.Linear.Test
import qualified Blucontrol.Value.RGB.Test

main :: IO ()
main = hspec $ do
  Blucontrol.Value.RGB.Test.test
  Blucontrol.Monad.PrepareValue.Linear.Test.test
