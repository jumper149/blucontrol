module Blumon.Main.CLI (
  launch
) where

import Control.DeepSeq
import Data.Version (showVersion)
import GHC.Generics
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Info (compilerName, compilerVersion)

import Paths_blumon (version)

data Flag = Help
          | Version
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData Flag

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Explain CLI usage"
          , Option ['v'] ["version"] (NoArg Version) "Display version"
          ]

launch :: IO ()
launch = do
  args <- getArgs
  case getOpt Permute options args of
    (optArgs, [], []) -> controlOptions optArgs
    _ -> do putStr $ usageInfo "Usage: blumon" options
            exitFailure

controlOptions :: [Flag] -> IO ()
controlOptions flags
  | Help `elem` flags = do printUsage
                           exitSuccess
  | otherwise = case flags of
    [] -> return ()
    [Version] -> do printVersion
                    exitSuccess
    _ -> do printUsage
            exitFailure

printUsage :: IO ()
printUsage = putStr $ usageInfo header options
  where header = "Usage: blumon [OPTIONS]"

-- TODO: don't hardcode version
printVersion :: IO ()
printVersion = putStrLn $ "blumon-" <> showVersion version <> " compiled with " <> compiler
  where compiler = compilerName <> "-" <> showVersion compilerVersion
