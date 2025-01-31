--------------------------------------------------------------------------
--- This module supports the configuration of the analysis system
--- and provides access to some values in Config file.
---
--- It also provides an operation to get the port number of
--- the analysis server (which is implicitly started if necessary).
---
--- @author Michael Hanus
--- @version January 2025
--------------------------------------------------------------------------

module CASS.Configuration
 ( systemBanner, baseDir, docDir, executableName
 , curryInfoAnalyses
 , CConfig(..), defaultCConfig, debugLevel, setDebugLevel
 , getServerAddress
 , useCurryInfo, useCurryInfoWeb, fixpointMethod, withPrelude
 , storeServerPortNumber, removeServerPortNumber
 , getDefaultPath, waitTime, numberOfWorkers
 ) where

import Control.Monad               ( unless, when )
import Curry.Compiler.Distribution ( curryCompiler )
import Data.List                   ( sort )
import Numeric                     ( readInt )
import System.Environment          ( getEnv )
import System.FilePath             ( (</>) )
import System.IO                   ( hPutStrLn, stderr )

import System.Directory   ( getHomeDirectory, removeFile )
import System.Process     ( getPID )

import Analysis.Logging   ( DLevel(..), debugMessage )
import CASS.Options       ( Options(..), defaultOptions )
import CASS.PackageConfig ( packagePath, packageExecutable, packageVersion )

--- The banner of the CASS system.
systemBanner :: String
systemBanner =
  let bannerText = "CASS: Curry Analysis Server System (Version " ++
                   packageVersion ++ " of 29/01/2025 for " ++
                   curryCompiler ++ ")"
      bannerLine = take (length bannerText) (repeat '=')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine


--- The base directory of the analysis tool containing all programs
--- and documentations.
--- It is used to copy the configuration file, to the find executables
--- of the server and the workers, and to find the documentation
--- of the various analyses.
baseDir :: String
baseDir = packagePath

--- The directory containing the documentations of the various analyses.
docDir :: String
docDir = baseDir </> "docs"

--- The name of the main executable. Used to start workers in `CASS.Server`.
executableName :: String
executableName = packageExecutable

--- The address of the server when it is connected from the worker clients.
getServerAddress :: IO String
getServerAddress = return "127.0.0.1" -- run only on local machine

--- timeout for network message passing: -1 is wait time infinity
waitTime :: Int
waitTime = -1

--- Default number of workers (if the number is not found in the
--- configuration file).
defaultWorkers :: Int
defaultWorkers = 0

--- Analysis names currently supported by CurryInfo.
curryInfoAnalyses :: [String]
curryInfoAnalyses =
  [ "Deterministic"
  , "Demand"
  , "Indeterministic"
  , "SolComplete"
  , "Terminating"
  , "Total"
  , "Values"
  ]

--------------------------------------------------------------------------
--- Configuration info used during execution of CASS.
--- It contains the properties from the RC file, the current debug level,
--- and the options passed to CASS.
data CConfig = CConfig
  { ccProps      :: [(String,String)]
  , ccDebugLevel :: DLevel
  , ccOptions    :: Options
  }

--- The default configuration has no properties and is quiet.
defaultCConfig :: CConfig
defaultCConfig = CConfig [] Quiet defaultOptions

--- Returns the debug level from the current configuration.
debugLevel :: CConfig -> DLevel
debugLevel = ccDebugLevel

--- Returns the debug level from the current configuration.
setDebugLevel :: Int -> CConfig -> CConfig
setDebugLevel dl cc = cc { ccDebugLevel = toEnum dl }

--- Returns the curryinfo flag from the current configuration.
useCurryInfo :: CConfig -> Bool
useCurryInfo cc =
  maybe False (`elem` ["local","remote"]) (lookup "curryinfo" (ccProps cc))

--- Returns the curryinfo web flag from the current configuration.
useCurryInfoWeb :: CConfig -> Bool
useCurryInfoWeb cc = maybe False (=="remote") (lookup "curryinfo" (ccProps cc))

--- Returns the fixpoint computation method from Config file
fixpointMethod :: CConfig -> String
fixpointMethod cc = maybe "wlist" id  (lookup "fixpoint" (ccProps cc))

--- Gets the option to analyze also the prelude from Config file
withPrelude :: CConfig -> Bool
withPrelude cc = maybe True (/="no") (lookup "prelude" (ccProps cc))

--- Gets the default load path from the property file (added at the end
--- of CURRYPATH).
getDefaultPath :: CConfig -> IO String
getDefaultPath cc = do
  currypath <- getEnv "CURRYPATH"
  return $ case lookup "path" (ccProps cc) of
    Just value -> if all isSpace value
                    then currypath
                    else if null currypath then value
                                           else currypath ++ ':' : value
    Nothing    -> currypath

--- number of worker threads running at the same time
numberOfWorkers :: CConfig -> Int
numberOfWorkers cc = do
  case lookup "numberOfWorkers" (ccProps cc) of
    Just value -> case readInt value of
                    [(int,_)] -> int
                    _         -> defaultWorkers
    Nothing    -> defaultWorkers

--------------------------------------------------------------------------
--- Gets the name of file containing the current server port and pid
--- ($HOME has to be set)
getServerPortFileName :: IO String
getServerPortFileName = (</> ".cass.port") `fmap` getHomeDirectory

--- Stores the current server port number together with the pid of
--- the server process.
storeServerPortNumber :: Int -> IO ()
storeServerPortNumber portnum = do
  mypid <- getPID
  serverPortFileName <- getServerPortFileName
  writeFile serverPortFileName (show (portnum,mypid))

--- Removes the currently stored server port number.
removeServerPortNumber :: IO ()
removeServerPortNumber = getServerPortFileName >>= removeFile

readServerPortPid :: IO (Int,Int)
readServerPortPid = getServerPortFileName >>= readFile >>= return . read

--------------------------------------------------------------------------
