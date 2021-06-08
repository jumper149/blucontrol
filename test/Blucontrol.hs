module Main where

import Test.Hspec

import qualified Blucontrol.Monad.Gamma.Linear.Test
import qualified Blucontrol.RGB.Test

main :: IO ()
main = hspec $ do
  Blucontrol.RGB.Test.test
  Blucontrol.Monad.Gamma.Linear.Test.test
