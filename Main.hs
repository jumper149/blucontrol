module Main where

import Blumon
import Blumon.Monad.Gamma.Const
import Blumon.Monad.Recolor.X

main :: IO ()
main = loopRecolor def $ runGammaConstT def . runRecolorXTIO
