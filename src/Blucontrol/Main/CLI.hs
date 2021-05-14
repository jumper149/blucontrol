module Blucontrol.Main.CLI (
  launch
) where

import Control.DeepSeq
import Control.Monad (when)
import Data.Version (showVersion)
import GHC.Generics
import System.Console.GetOpt
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getModificationTime, getXdgDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.Info (arch, compilerName, compilerVersion, os)
import System.Posix.Process (executeFile)
import System.Process (runProcess, waitForProcess)

import Blucontrol.Main.GHC.Internal
import Paths_blucontrol (version)

data Flag = Help
          | Version
          | IgnoreConfig
  deriving (Eq, Generic, Ord, Read, Show)

instance NFData Flag

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Explain CLI usage"
          , Option ['v'] ["version"] (NoArg Version) "Display version"
          , Option ['i'] ["ignore-config"] (NoArg IgnoreConfig) "Use default configuration"
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
    [IgnoreConfig] -> return ()
    _ -> do printUsage
            exitFailure

printUsage :: IO ()
printUsage = putStr $ usageInfo header options
  where header = "Usage: blucontrol [OPTIONS]"

printVersion :: IO ()
printVersion = putStrLn $ "blucontrol-" <> showVersion version <> " compiled with " <> compiler
  where compiler = compilerName <> "-" <> showVersion compilerVersion

getXdgDir :: XdgDirectory -> IO FilePath
getXdgDir = flip getXdgDirectory "blucontrol"

build :: IO ()
build = do
  configPath <- (</> configLeafname) <$> getXdgDir XdgConfig
  configExists <- doesFileExist configPath
  when configExists $ do
    progName <- getProgName
    compiledConfigPath <- (</> compiledConfigLeafname) <$> getXdgDir XdgCache
    if progName == compiledConfigLeafname
       then do configTime <- getModificationTime configPath -- TODO: getModificationTime can fail
               compiledConfigTime <- getModificationTime compiledConfigPath
               when (configTime > compiledConfigTime) $ do
                 compile
                 executeFile compiledConfigPath False [] Nothing
       else do compile
               executeFile compiledConfigPath False [] Nothing

compile :: IO ()
compile = do
  configDir <- getXdgDir XdgConfig
  cacheDir <- getXdgDir XdgCache
  createDirectoryIfMissing False cacheDir
  let ghcFlags = [ "--make"
                 , configLeafname
                 , "-main-is", "main"
                 , "-v0"
                 , "-o", cacheDir </> compiledConfigLeafname
                 ] <> ghcAdditionalFlags
  status <- waitForProcess =<<
    runProcess ghcBinary ghcFlags (Just configDir) Nothing Nothing Nothing Nothing
  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> exitFailure

compiledConfigLeafname :: FilePath
compiledConfigLeafname = "blucontrol-" <> arch <> "-" <> os

configLeafname :: FilePath
configLeafname = "blucontrol.hs"
