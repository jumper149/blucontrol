{-# LANGUAGE RecordWildCards, UndecidableInstances #-}

module Blumon.Monad.Recolor.X (
  RecolorXT
, runRecolorXT
) where

import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Graphics.X11.Xlib.Display (closeDisplay, defaultScreen, openDisplay, rootWindow)
import Graphics.X11.Xlib.Types (Display)

import Blumon.RGB
import Blumon.Monad.Gamma
import Blumon.Monad.Recolor
import Blumon.Monad.Recolor.X.Internal

newtype RecolorXT m a = RecolorXT { unRecolorXT :: ReaderT Display m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadGamma, MonadTrans)

instance (MonadBase IO m, MonadGamma m) => MonadRecolor (RecolorXT m) where
  recolor = do display <- RecolorXT ask
               root <- liftBase $ rootWindow display $ defaultScreen display

               rgb <- translateRGB <$> lift gamma
               liftBase $ xrrSetGamma rgb display root

runRecolorXT :: MonadBase IO m => RecolorXT m a -> m a
runRecolorXT tma = do
  display <- liftBase $ openDisplay ""
  a <- runReaderT (unRecolorXT tma) display
  liftBase $ closeDisplay display
  return a

translateRGB :: Trichromaticity -> XRRGamma
translateRGB Trichromaticity {..} = XRRGamma {..}
  where xrr_gamma_red = translateColor red
        xrr_gamma_green = translateColor green
        xrr_gamma_blue = translateColor blue

-- | Create a normalized value for a 'Chromaticity'.
translateColor :: (Fractional a, Num a) => Chromaticity -> a
translateColor = (/ fromIntegral (maxBound @Chromaticity)) . fromIntegral
