module Main where

import Test.Hspec

import qualified Bludigon.Test.Gamma.Linear
import qualified Bludigon.Test.RGB

main :: IO ()
main = hspec $ do
  Bludigon.Test.RGB.test
  Bludigon.Test.Gamma.Linear.test
