module Blucontrol.Monad.Recolor (
  MonadRecolor (..)
) where

class Monad m => MonadRecolor m where

  type RecolorValue m

  {- | Apply a gamma value.
     This is a monadic function, to allow application to external programs like an X display
     server.
  -}
  recolor :: RecolorValue m -> m ()
