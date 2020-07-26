module Blumon.Config (
  Config (..)
) where

import Data.Default
import GHC.Generics

data Config = Config { configDir :: FilePath
                     }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default Config where
  def = Config { configDir = ".config/blumon" 
               }
