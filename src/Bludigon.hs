module Bludigon (

-- * main
{- | For most configurations 'bludigon' should be called directly from the @main@ function in
   @$XDG_CONFIG_HOME\/bludigon\/bludigon.hs@
-}
  bludigon
{- | 'ConfigControl' will set the monads in which recoloring and calculation of the gamma values
  will take place.
-}
, ConfigControl (..)

-- * RGB
-- | RGB values are represented by 'Trichromaticity'.
, Trichromaticity (..)

{- | An alternative way to declaring 'Trichromaticity' directly is to use 'Temperature' and the
  conversion function 'temperature'.
-}
, Temperature
, temperature

-- * Control
-- | Modules with instances of 'MonadControl' can be found under @Bludigon.Control.*@.
, MonadControl (..)

{- | To compose instances of 'MonadControl' avoid function composition, as it won't compose
   'doInbetween'.
   Use '!>' instead.
-}
, (!>)

-- * Gamma
-- | Modules with instances of 'MonadGamma' can be found under @Bludigon.Gamma.*@.
, MonadGamma (..)

-- * Recolor
-- | Modules with instances of 'MonadRecolor' can be found under @Bludigon.Recolor.*@.
, MonadRecolor (..)

-- * other
, Default (..)
) where

import Data.Default

import Bludigon.Control
import Bludigon.Control.Concat
import Bludigon.Gamma
import Bludigon.Main
import Bludigon.Recolor
import Bludigon.RGB
import Bludigon.RGB.Temperature
