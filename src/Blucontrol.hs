module Blucontrol (

-- * main
{- | For most configurations 'blucontrol' should be called directly from the @main@ function in
   @$XDG_CONFIG_HOME\/blucontrol\/blucontrol.hs@
-}
  blucontrol
, BlucontrolConstraints
{- | 'ConfigControl' will set the monads in which recoloring and calculation of the gamma values
  will take place.
-}
, ConfigControl (..)

-- * Control
-- | Modules with instances of 'MonadControl' can be found under @Blucontrol.Control.*@.
, MonadControl (..)

{- | To compose instances of 'MonadControl' avoid function composition, as it won't compose
   'doInbetween'.
   Use '!>' instead.
-}
, (!>)

-- * Gamma
-- | Modules with instances of 'MonadGamma' can be found under @Blucontrol.Gamma.*@.
, MonadGamma (..)

-- * Recolor
-- | Modules with instances of 'MonadRecolor' can be found under @Blucontrol.Recolor.*@.
, MonadRecolor (..)

-- * other
, Default (..)
) where

import Data.Default

import Blucontrol.Main
import Blucontrol.Monad.Control
import Blucontrol.Monad.Control.Concat
import Blucontrol.Monad.Gamma
import Blucontrol.Monad.Recolor
