{-# LANGUAGE UndecidableInstances #-}

module Blumon.Monad.Recolor.X (
  RecolorXT
  runRecolorXT
) where

import Control.Monad.Base
import Control.Monad.Trans.Class

import Blumon.Monad.Gamma
import Blumon.Monad.Recolor

newtype RecolorXT m a = RecolorXT { unRecolorXT :: m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadGamma)

instance MonadTrans RecolorXT where
  lift = RecolorXT

instance (MonadBase IO m, MonadGamma m) => MonadRecolor (RecolorXT m) where
  recolor = do rgb <- gamma
               liftBase $ print rgb -- TODO

runRecolorXT :: RecolorXT m a -> m a
runRecolorXT = unRecolorXT
