module Main where

import Blumon
import Blumon.Control.Print
import Blumon.Control.Wait
import Blumon.Gamma.Linear
import Blumon.Recolor.X

main :: IO ()
main = blumon def configControl
  where configControl = ConfigControl { runControl = runControlWaitT def . runControlPrintT
                                      , runGamma = runGammaLinearT rgbMap
                                      , runRecolor = runRecolorXTIO def
                                      }
        rgbMap = 00:.00 ==> temperature 6600
            :| [ 08:.00 ==> temperature 4600
               , 16:.00 ==> temperature 8600
               ]
