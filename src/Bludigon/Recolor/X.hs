{-# LANGUAGE RecordWildCards, UndecidableInstances #-}

module Bludigon.Recolor.X (
  RecolorXT
, runRecolorXTIO
, ConfigX (..)
, XError (..)
) where

import Control.DeepSeq
import Control.Exception.Lifted (SomeException (..), bracket, catch)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.Except
import Data.Default
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics

import Graphics.X11.Xlib.Display (closeDisplay, defaultScreen, openDisplay, rootWindow)
import Graphics.X11.Xlib.Types (Display)

import Bludigon.RGB
import Bludigon.Recolor
import Bludigon.Recolor.X.Internal

newtype RecolorXT m a = RecolorXT { unRecolorXT :: ExceptT XError (ReaderT Display m) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadError XError)

instance MonadTrans RecolorXT where
  lift = RecolorXT . lift . lift

instance MonadTransControl RecolorXT where
  type StT RecolorXT a = StT (ReaderT Display) (StT (ExceptT XError) a)
  liftWith = defaultLiftWith2 RecolorXT unRecolorXT
  restoreT = defaultRestoreT2 RecolorXT

instance MonadBaseControl IO m => MonadRecolor (RecolorXT m) where
  recolor rgb = do
    display <- RecolorXT ask
    root <- liftXIO XErrorRead $
      rootWindow display $ defaultScreen display

    liftXIO XErrorSetGamma $ xrrSetGamma (translateRGB rgb) display root

runRecolorXT :: Display -> RecolorXT m a -> m (Either XError a)
runRecolorXT display tma = runReaderT (runExceptT (unRecolorXT tma)) display

data ConfigX = ConfigX { hostName :: Maybe T.Text
                       , displayServer :: Int
                       , screen :: Maybe Int
                       }
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData ConfigX

instance Default ConfigX where
  def = ConfigX { hostName = Nothing
                , displayServer = 0
                , screen = Nothing
                }

data XError = XErrorCloseDisplay
            | XErrorOpenDisplay
            | XErrorRead
            | XErrorSetGamma
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance NFData XError

liftXIO :: (MonadBaseControl IO m, MonadError XError m) => XError -> IO a -> m a
liftXIO xError = (flip catch $ \ (SomeException _) -> throwError xError) . liftBase

runRecolorXTIO :: MonadBaseControl IO m => ConfigX -> RecolorXT m a -> m (Either XError a)
runRecolorXTIO conf tma = runExceptT $ bracket open close run
  where open = liftXIO XErrorOpenDisplay $ openDisplay $ showDisplay conf
        close display = liftXIO XErrorCloseDisplay $ closeDisplay display
        run display = restoreT $ runRecolorXT display tma

showDisplay :: ConfigX -> String
showDisplay ConfigX {..} = T.unpack . T.concat $
  [ fromMaybe "" hostName
  , ":" <> T.pack (show displayServer)
  , maybe "" (("." <>) . T.pack . show) screen
  ]

translateRGB :: Trichromaticity -> XRRGamma
translateRGB Trichromaticity {..} = XRRGamma {..}
  where xrr_gamma_red = translateColor red
        xrr_gamma_green = translateColor green
        xrr_gamma_blue = translateColor blue

-- | Create a normalized value for a 'Chromaticity'.
translateColor :: (Fractional a, Num a) => Chromaticity -> a
translateColor = (/ fromIntegral (maxBound @Chromaticity)) . fromIntegral
