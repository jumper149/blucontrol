module Main where

import Blumon.RGB
import Blumon.Monad.Gamma.Const
import Blumon.Monad.Recolor
import Blumon.Monad.Recolor.X

main :: IO ()
main = runGammaConstT rgb . runRecolorXT $ recolor
  where rgb = Trichromaticity { red = 255
                              , green = 255
                              , blue = 255
                              }
