module Blucontrol.CompatibleValues (
  CompatibleValues (..)
) where

class CompatibleValues a b where
    convertValue :: a -> b

instance CompatibleValues a a where
  convertValue = id
