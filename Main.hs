{-# LANGUAGE TypeApplications #-}

module Main where

import Blucontrol
import Blucontrol.Control.Count
import Blucontrol.Control.Print
import Blucontrol.Control.Wait
import Blucontrol.Gamma.Linear
import Blucontrol.Recolor.X
import Blucontrol.RGB
import Blucontrol.RGB.Temperature

import Data.Word

main :: IO ()
main = blucontrol configControl
  where configControl = ConfigControl { runControl = runControlPrintT !> runControlCountT def !> runControlWaitT def
                                      , runGamma = runGammaLinearT @Temperature rgbMap
                                      , runRecolor = runRecolorXTIO def
                                      , coerceRGB = mapRGB normalize . toRGBWord8
                                      }
        rgbMap = 00:.00 ==> 4000
            :| [ 08:.00 ==> 4600
               , 12:.00 ==> 6600
               , 18:.00 ==> 6000
               ]

normalize :: Word8 -> Float
normalize = (/ fromIntegral (maxBound @Word8)) . fromIntegral
