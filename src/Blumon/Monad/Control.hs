{-# LANGUAGE UndecidableInstances #-}

module Blumon.Monad.Control (
  MonadControl (..)
) where

import qualified Streamly as S

import Blumon.Config

class S.MonadAsync m => MonadControl m where
  doInbetween :: Config -> a -> m ()
