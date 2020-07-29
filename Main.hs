module Main where

import Blumon
import Blumon.Control.Print
import Blumon.Control.Wait
import Blumon.Gamma.Linear
import Blumon.Recolor.X

main :: IO ()
main = blumon def configControl
  where configControl = ConfigControl { runControl = runControlWaitT def . runControlPrintT
                                      , runRecolor = runGammaLinearT rgbMap . runRecolorXTIO def
                                      }
        rgbMap = 00:.00 ==> Trichromaticity { red = 255, green = 255, blue = 000 }
            :| [ 08:.00 ==> Trichromaticity { red = 255, green = 000, blue = 255 }
               , 16:.00 ==> Trichromaticity { red = 000, green = 255, blue = 255 }
               ]
