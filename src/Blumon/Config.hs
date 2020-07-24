module Blumon.Config (
  Config (..)
) where

import Data.Default
import GHC.Generics

type Microseconds = Int

data Config = Config { configDir :: FilePath
                     , interval :: Microseconds
                     }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default Config where
  def = Config { configDir = ".config/blumon" 
               , interval = 1000000
               }
