module Blucontrol.Main.Control (
  loopRecolor
, ConfigControl (..)
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State.Strict

import Blucontrol.Control
import Blucontrol.Gamma
import Blucontrol.Recolor
import Blucontrol.RGB

loopRecolor :: (ControlConstraint m (StM g (StM r ())), MonadBaseControl IO g, MonadBaseControl IO r, MonadControl m, MonadGamma c g, MonadRecolor r)
            => (forall a. g a -> IO (StM g a))
            -> (forall a. r a -> IO (StM r a))
            -> m ()
loopRecolor runG runR = void $
  liftBaseWith $ \ runCIO ->
    runR $ liftBaseWith $ \ runRIO ->
      runG $ liftBaseWith $ \ runGIO -> do
        let doRecolorGamma = do
              runGIO $ do
                rgb <- toRGB <$> gamma
                liftBase $ runRIO $ recolor rgb
            doLoopRecolor = do
              lastResult <- get
              _ <- liftBase $ runCIO $ doInbetween lastResult
              nextResult <- liftBase doRecolorGamma
              put nextResult
              doLoopRecolor
        firstResult <- doRecolorGamma
        evalStateT doLoopRecolor firstResult


data ConfigControl m g r = ConfigControl { runControl :: forall a. m a -> IO a
                                         , runGamma   :: forall a. g a -> IO (StM g a)
                                         , runRecolor :: forall a. r a -> IO (StM r a)
                                         }
