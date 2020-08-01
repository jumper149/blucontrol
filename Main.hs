module Main where

import Bludigon
import Bludigon.Control.Print
import Bludigon.Control.Wait
import Bludigon.Gamma.Linear
import Bludigon.Recolor.X

main :: IO ()
main = bludigon def configControl
  where configControl = ConfigControl { runControl = runControlWaitT def . runControlPrintT
                                      , runGamma = runGammaLinearT rgbMap
                                      , runRecolor = runRecolorXTIO def
                                      }
        rgbMap = 00:.00 ==> temperature 6600
            :| [ 08:.00 ==> temperature 4600
               , 16:.00 ==> temperature 8600
               ]
