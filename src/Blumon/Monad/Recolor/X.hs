{-# LANGUAGE RecordWildCards, UndecidableInstances #-}

module Blumon.Monad.Recolor.X (
  RecolorXT
, runRecolorXTIO
, XError (..)
) where

import Control.Exception.Lifted (SomeException (..), bracket, catch)
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.Except
import GHC.Generics

import Graphics.X11.Xlib.Display (closeDisplay, defaultScreen, openDisplay, rootWindow)
import Graphics.X11.Xlib.Types (Display)

import Blumon.RGB
import Blumon.Monad.Gamma
import Blumon.Monad.Recolor
import Blumon.Monad.Recolor.X.Internal

newtype RecolorXT m a = RecolorXT { unRecolorXT :: ExceptT XError (ReaderT Display m) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadError XError, MonadGamma)

instance MonadTrans RecolorXT where
  lift = RecolorXT . lift . lift

instance MonadTransControl RecolorXT where
  type StT RecolorXT a = StT (ReaderT Display) (StT (ExceptT XError) a)
  liftWith = defaultLiftWith2 RecolorXT unRecolorXT
  restoreT = defaultRestoreT2 RecolorXT

instance (MonadBaseControl IO m, MonadGamma m) => MonadRecolor (RecolorXT m) where
  recolor = do display <- RecolorXT ask
               root <- liftXIO XErrorRead $
                 rootWindow display $ defaultScreen display

               rgb <- translateRGB <$> gamma
               liftXIO XErrorSetGamma $ xrrSetGamma rgb display root

runRecolorXT :: Display -> RecolorXT m a -> m (Either XError a)
runRecolorXT display tma = runReaderT (runExceptT (unRecolorXT tma)) display

data XError = XErrorCloseDisplay
            | XErrorOpenDisplay
            | XErrorRead
            | XErrorSetGamma
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

liftXIO :: (MonadBaseControl IO m, MonadError XError m) => XError -> IO a -> m a
liftXIO xError = (flip catch $ \ (SomeException _) -> throwError xError) . liftBase

runRecolorXTIO :: MonadBaseControl IO m => RecolorXT m a -> m (Either XError a)
runRecolorXTIO tma = runExceptT $ bracket open close run
  where open = liftXIO XErrorOpenDisplay $ openDisplay ""
        close display = liftXIO XErrorCloseDisplay $ closeDisplay display
        run display = restoreT $ runRecolorXT display tma

translateRGB :: Trichromaticity -> XRRGamma
translateRGB Trichromaticity {..} = XRRGamma {..}
  where xrr_gamma_red = translateColor red
        xrr_gamma_green = translateColor green
        xrr_gamma_blue = translateColor blue

-- | Create a normalized value for a 'Chromaticity'.
translateColor :: (Fractional a, Num a) => Chromaticity -> a
translateColor = (/ fromIntegral (maxBound @Chromaticity)) . fromIntegral
