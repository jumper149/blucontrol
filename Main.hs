module Main where

import Blumon
import Blumon.Monad.Control.Wait
import Blumon.Monad.Gamma.Linear
import Blumon.Monad.Recolor.Print

main :: IO ()
main = blumon def configControl
  where configControl = ConfigControl { runControl = runControlWaitT def
                                      , runRecolor = runGammaLinearT rgbMap . runRecolorPrintT
                                      }
        rgbMap = 00:.00 ==> Trichromaticity { red = 255, green = 255, blue = 200 }
            :| [ 08:.00 ==> Trichromaticity { red = 255, green = 200, blue = 255 }
               , 16:.00 ==> Trichromaticity { red = 200, green = 255, blue = 255 }
               ]
