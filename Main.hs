{-# LANGUAGE TypeApplications #-}

module Main where

import Blucontrol
import Blucontrol.Monad.Control.Count
import Blucontrol.Monad.Control.Print
import Blucontrol.Monad.Control.Wait
import Blucontrol.Monad.PrepareValue.Linear
import Blucontrol.Monad.Recolor.X
import Blucontrol.Value.RGB.Temperature

main :: IO ()
main = print =<< blucontrol configControl
  where configControl = ConfigControl { runControl = runControlPrintT !> runControlCountT def !> runControlWaitT def
                                      , runPrepareValue = runPrepareValueLinearT @Temperature temperatureMap
                                      , runRecolor = runRecolorXTIO def
                                      }
        temperatureMap = 00:.00 ==> 4000
                    :| [ 08:.00 ==> 4600
                       , 12:.00 ==> 6600
                       , 18:.00 ==> 6000
                       ]
