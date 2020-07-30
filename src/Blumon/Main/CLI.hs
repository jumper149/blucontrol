module Blumon.Main.CLI (
  launch
) where

import Control.DeepSeq
import Data.Version (showVersion)
import GHC.Generics
import System.Console.GetOpt
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.Info (arch, compilerName, compilerVersion, os)
import System.Posix.Process (executeFile)
import System.Process (runProcess, waitForProcess)

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
    _ -> do printUsage
            exitFailure

controlOptions :: [Flag] -> IO ()
controlOptions flags
  | Help `elem` flags = do printUsage
                           exitSuccess
  | otherwise = case flags of
    [] -> build
    [Version] -> do printVersion
                    exitSuccess
    _ -> do printUsage
            exitFailure

printUsage :: IO ()
printUsage = putStr $ usageInfo header options
  where header = "Usage: blumon [OPTIONS]"

printVersion :: IO ()
printVersion = putStrLn $ "blumon-" <> showVersion version <> " compiled with " <> compiler
  where compiler = compilerName <> "-" <> showVersion compilerVersion

getXdgDir :: XdgDirectory -> IO FilePath
getXdgDir = flip getXdgDirectory "blumon"

build :: IO ()
build = do
  configPath <- (</> configLeafname) <$> getXdgDir XdgConfig
  configExists <- doesFileExist configPath
  if configExists
     then do progName <- getProgName
             if progName == compiledConfigLeafname
                then return ()
                else do compile
                        cacheDir <- getXdgDir XdgCache
                        executeFile (cacheDir </> compiledConfigLeafname) False [] Nothing
     else return ()

compile :: IO ()
compile = do
  configDir <- getXdgDir XdgConfig
  cacheDir <- getXdgDir XdgCache
  createDirectoryIfMissing False cacheDir
  status <- waitForProcess =<<
    runProcess "ghc" [ "--make"
                     , configLeafname
                     , "-main-is", "main"
                     , "-v0"
                     , "-o", cacheDir </> compiledConfigLeafname
                     ] (Just configDir) Nothing Nothing Nothing Nothing
  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> exitFailure

compiledConfigLeafname :: FilePath
compiledConfigLeafname = "blumon-" <> arch <> "-" <> os

configLeafname :: FilePath
configLeafname = "blumon.hs"
