{-# LANGUAGE UndecidableInstances #-}

module Blumon.Monad.Control (
  MonadControl (..)
) where

import qualified Streamly as S

class S.MonadAsync m => MonadControl m where
  doInbetween :: a -> m ()
