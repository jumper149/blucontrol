module Main where

import qualified Blumon.Test.Monad.Gamma.Linear
import qualified Blumon.Test.RGB

main :: IO ()
main = hspec $ do
  Blumon.Test.RGB.test
  Blumon.Test.Monad.Gamma.Linear.test
