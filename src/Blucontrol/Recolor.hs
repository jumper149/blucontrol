module Blucontrol.Recolor (
  MonadRecolor (..)
) where

class Monad m => MonadRecolor m where

  type RecolorRGB m

  {- | Apply a gamma value.
     This is a monadic function, to allow application to external programs like an X display
     server.
  -}
  recolor :: RecolorRGB m -> m ()
