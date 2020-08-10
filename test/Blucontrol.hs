module Main where

import Test.Hspec

import qualified Blucontrol.Test.Gamma.Linear
import qualified Blucontrol.Test.RGB

main :: IO ()
main = hspec $ do
  Blucontrol.Test.RGB.test
  Blucontrol.Test.Gamma.Linear.test
