{-# LANGUAGE UndecidableInstances #-}

module Blucontrol.Monad.ApplyValue.X (
  ApplyValueXT
, runApplyValueXTIO
, ApplicableValueX
, ConfigX (..)
, XError (..)
) where

import Control.DeepSeq
import Control.Exception.Lifted (SomeException (..), bracket, catch)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Default
import Control.Monad.Reader
import Control.Monad.Except
import Data.Default
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word
import GHC.Generics

import Graphics.X11.Xlib.Display (closeDisplay, defaultScreen, openDisplay, rootWindow)
import Graphics.X11.Xlib.Types (Display)

import Blucontrol.Monad.ApplyValue
import Blucontrol.Monad.ApplyValue.X.Internal
import Blucontrol.Value
import Blucontrol.Value.RGB

newtype ApplyValueXT m a = ApplyValueXT { unApplyValueXT :: ExceptT XError (ReaderT Display m) a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadBaseControl b, MonadError XError)
  deriving (MonadTrans, MonadTransControl) via Stack2T (ExceptT XError) (ReaderT Display)

instance MonadBaseControl IO m => MonadApplyValue (ApplyValueXT m) where
  type ApplicableValue (ApplyValueXT m) = ApplicableValueX
  applyValue rgb = do
    display <- ApplyValueXT ask
    root <- liftXIO XErrorRead $
      rootWindow display $ defaultScreen display

    liftXIO XErrorSetGamma $ xrrSetGamma (unApplicableValueX rgb) display root

runApplyValueXT :: Display -> ApplyValueXT m a -> m (Either XError a)
runApplyValueXT display tma = runReaderT (runExceptT (unApplyValueXT tma)) display

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

runApplyValueXTIO :: MonadBaseControl IO m => ConfigX -> ApplyValueXT m a -> m (Either XError a)
runApplyValueXTIO !conf tma = runExceptT $ bracket open close run
  where open = liftXIO XErrorOpenDisplay $ openDisplay $ showDisplay conf
        close display = liftXIO XErrorCloseDisplay $ closeDisplay display
        run display = restoreT $ runApplyValueXT display tma

showDisplay :: ConfigX -> String
showDisplay ConfigX { hostName, displayServer, screen } = T.unpack . T.concat $
  [ fromMaybe "" hostName
  , ":" <> T.pack (show displayServer)
  , maybe "" (("." <>) . T.pack . show) screen
  ]

newtype ApplicableValueX = ApplicableValueX { unApplicableValueX :: XRRGamma }
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData ApplicableValueX

instance CompatibleValues (RGB Word8) ApplicableValueX where
  convertValue RGB { red, green, blue } = ApplicableValueX XRRGamma { xrr_gamma_red, xrr_gamma_green, xrr_gamma_blue }
    where xrr_gamma_red = word8ToFloat red
          xrr_gamma_green = word8ToFloat green
          xrr_gamma_blue = word8ToFloat blue
          word8ToFloat = (/ fromIntegral (maxBound @Word8)) . fromIntegral
