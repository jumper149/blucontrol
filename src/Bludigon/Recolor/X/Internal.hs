{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}

module Bludigon.Recolor.X.Internal (
  XRRGamma (..)
, xrrSetGamma
) where

import Control.DeepSeq
import Foreign.Ptr
import GHC.Generics

import Graphics.X11.Types
import Graphics.X11.Xlib.Types

data XRRGamma = XRRGamma { xrr_gamma_red :: Float
                         , xrr_gamma_green :: Float
                         , xrr_gamma_blue :: Float
                         }
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData XRRGamma

xrrSetGamma :: XRRGamma -> Display -> Window -> IO ()
xrrSetGamma XRRGamma {..} (Display display) window = do
  res <- _XRRGetScreenResourcesCurrent display window
  _setGamma xrr_gamma_red xrr_gamma_green xrr_gamma_blue res display

foreign import ccall "XrandrGamma.h setGamma" _setGamma :: Float -> Float -> Float -> Ptr Int -> Ptr Display -> IO ()
foreign import ccall "<X11/extensions/Xrandr.h> XRRGetScreenResourcesCurrent" _XRRGetScreenResourcesCurrent :: Ptr Display -> Window -> IO (Ptr Int)
