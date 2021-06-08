{-# LANGUAGE RecordWildCards, UndecidableInstances #-}

module Blucontrol.Monad.Recolor.X (
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

import Blucontrol.Monad.Recolor
import Blucontrol.Monad.Recolor.X.Internal
import Blucontrol.RGB

newtype RecolorXT m a = RecolorXT { unRecolorXT :: ExceptT XError (ReaderT Display m) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadError XError)

instance MonadTrans RecolorXT where
  lift = RecolorXT . lift . lift

instance MonadTransControl RecolorXT where
  type StT RecolorXT a = StT (ReaderT Display) (StT (ExceptT XError) a)
  -- TODO: broken by ghc-9.0.1
  -- liftWith = defaultLiftWith2 RecolorXT unRecolorXT
  liftWith f = RecolorXT $ liftWith $ \run -> liftWith $ \run' -> f $ run' . run . unRecolorXT
  restoreT = defaultRestoreT2 RecolorXT

instance MonadBaseControl IO m => MonadRecolor (RecolorXT m) where
  type RecolorValue (RecolorXT m) = RGB Float
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
liftXIO xError = flip catch throwXError . liftBase
  where throwXError (SomeException _) = throwError xError

runRecolorXTIO :: MonadBaseControl IO m => ConfigX -> RecolorXT m a -> m (Either XError a)
runRecolorXTIO !conf tma = runExceptT $ bracket open close run
  where open = liftXIO XErrorOpenDisplay $ openDisplay $ showDisplay conf
        close display = liftXIO XErrorCloseDisplay $ closeDisplay display
        run display = restoreT $ runRecolorXT display tma

showDisplay :: ConfigX -> String
showDisplay ConfigX {..} = T.unpack . T.concat $
  [ fromMaybe "" hostName
  , ":" <> T.pack (show displayServer)
  , maybe "" (("." <>) . T.pack . show) screen
  ]

translateRGB :: RGB Float -> XRRGamma
translateRGB RGB {..} = XRRGamma {..}
  where xrr_gamma_red = red
        xrr_gamma_green = green
        xrr_gamma_blue = blue