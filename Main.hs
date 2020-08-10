module Main where

import Bludigon
import Bludigon.Control.Count
import Bludigon.Control.Print
import Bludigon.Control.Wait
import Bludigon.Gamma.Linear
import Bludigon.Recolor.X

main :: IO ()
main = bludigon configControl
  where configControl = ConfigControl { runControl = runControlPrintT !> runControlCountT def !> runControlWaitT def
                                      , runGamma = runGammaLinearT rgbMap
                                      , runRecolor = runRecolorXTIO def
                                      }
        rgbMap = 00:.00 ==> temperature 4000
            :| [ 08:.00 ==> temperature 4600
               , 12:.00 ==> temperature 6600
               , 18:.00 ==> temperature 6000
               ]
