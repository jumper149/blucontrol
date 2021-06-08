{-# LANGUAGE TypeApplications #-}

module Main where

import Blucontrol
import Blucontrol.Monad.Control.Count
import Blucontrol.Monad.Control.Print
import Blucontrol.Monad.Control.Wait
import Blucontrol.Monad.Gamma.Linear
import Blucontrol.Monad.Recolor.X
import Blucontrol.Value.RGB.Temperature

main :: IO ()
main = blucontrol configControl
  where configControl = ConfigControl { runControl = runControlPrintT !> runControlCountT def !> runControlWaitT def
                                      , runGamma = runGammaLinearT @Temperature rgbMap
                                      , runRecolor = runRecolorXTIO def
                                      }
        rgbMap = 00:.00 ==> 4000
            :| [ 08:.00 ==> 4600
               , 12:.00 ==> 6600
               , 18:.00 ==> 6000
               ]
