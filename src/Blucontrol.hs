module Blucontrol (

-- * main
{- | For most configurations 'blucontrol' should be called directly from the @main@ function in
   @$XDG_CONFIG_HOME\/blucontrol\/blucontrol.hs@
-}
  blucontrol
, BlucontrolConstraints
{- | 'ConfigControl' will set the monads in which calculation and application of values will take
   place.
-}
, ConfigControl (..)

-- * Control
-- | Modules with instances of 'MonadControl' can be found under @Blucontrol.Monad.Control.*@.
, MonadControl (..)

{- | To compose instances of 'MonadControl' avoid function composition, as it won't compose
   'doInbetween'.
   Use '!>' instead.
-}
, (!>)

-- * Prepare value
-- | Modules with instances of 'MonadPrepareValue' can be found under @Blucontrol.Monad.PrepareValue.*@.
, MonadPrepareValue (..)

-- * ApplyValue
-- | Modules with instances of 'MonadApplyValue' can be found under @Blucontrol.Monad.ApplyValue.*@.
, MonadApplyValue (..)

-- * other
, Default (..)
) where

import Data.Default

import Blucontrol.Main
import Blucontrol.Monad.ApplyValue
import Blucontrol.Monad.Control
import Blucontrol.Monad.Control.Concat
import Blucontrol.Monad.PrepareValue
