module Main where

import Test.Hspec

import qualified Blumon.Test.Gamma.Linear
import qualified Blumon.Test.RGB

main :: IO ()
main = hspec $ do
  Blumon.Test.RGB.test
  Blumon.Test.Gamma.Linear.test
