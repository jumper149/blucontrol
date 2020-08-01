module Bludigon.Config (
  Config (..)
) where

import Control.DeepSeq
import Data.Default
import GHC.Generics

data Config = Config { configDir :: FilePath
                     }
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData Config

instance Default Config where
  def = Config { configDir = ".config/blumon" 
               }
