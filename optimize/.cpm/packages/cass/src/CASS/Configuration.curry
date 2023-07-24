--------------------------------------------------------------------------
--- This module supports the configuration of the analysis system
--- and provides access to some values in Config file.
---
--- It also provides an operation to get the port number of
--- the analysis server (which is implicitly started if necessary).
---
--- @author Michael Hanus
--- @version February 2023
--------------------------------------------------------------------------

module CASS.Configuration
 ( systemBanner, baseDir, docDir, executableName
 , CConfig, defaultCConfig, debugLevel, setDebugLevel
 , getServerAddress, readRCFile, updateProperty
 , fixpointMethod, withPrelude
 , storeServerPortNumber, removeServerPortNumber
 , getDefaultPath, waitTime, numberOfWorkers
 ) where

import Control.Monad               ( unless )
import Curry.Compiler.Distribution ( curryCompiler )
import Data.List                   ( sort )
import Numeric                     ( readInt )
import System.Environment          ( getEnv )
import System.FilePath             ( FilePath, (</>), (<.>) )

import System.Process
import System.Directory

import Analysis.Logging   ( DLevel(..) )
import CASS.PackageConfig ( packagePath, packageExecutable, packageVersion )
import Data.PropertyFile  ( readPropertyFile, updatePropertyFile )

systemBanner :: String
systemBanner =
  let bannerText = "CASS: Curry Analysis Server System (Version " ++
                   packageVersion ++ " of 26/06/2023 for " ++
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

-- timeout for network message passing: -1 is wait time infinity
waitTime :: Int
waitTime = -1

-- Default number of workers (if the number is not found in the
-- configuration file).
defaultWorkers :: Int
defaultWorkers = 0

--------------------------------------------------------------------------
--- Configuration info used during execution of CASS.
--- It contains the properties from the rc file and the current debug level.
data CConfig = CConfig [(String,String)] DLevel

defaultCConfig :: CConfig
defaultCConfig = CConfig [] Quiet

--- Returns the debug level from the current configuration.
debugLevel :: CConfig -> DLevel
debugLevel (CConfig _ dl) = dl

--- Returns the debug level from the current configuration.
setDebugLevel :: Int -> CConfig -> CConfig
setDebugLevel dl (CConfig ps _) = CConfig ps (toEnum dl)

-- Returns the fixpoint computation method from Config file
fixpointMethod :: CConfig -> String
fixpointMethod (CConfig properties _) =
  maybe "simple" id  (lookup "fixpoint" properties)

-- Get the option to analyze also the prelude from Config file
withPrelude :: CConfig -> Bool
withPrelude (CConfig properties _) =
  maybe True (/="no") (lookup "prelude" properties)

--- Gets the default load path from the property file (added at the end
--- of CURRYPATH).
getDefaultPath :: CConfig -> IO String
getDefaultPath (CConfig properties _) = do
  currypath <- getEnv "CURRYPATH"
  return $ case lookup "path" properties of
    Just value -> if all isSpace value
                    then currypath
                    else if null currypath then value
                                           else currypath ++ ':' : value
    Nothing    -> currypath

-- number of worker threads running at the same time
numberOfWorkers :: CConfig -> Int
numberOfWorkers (CConfig properties _) = do
  case lookup "numberOfWorkers" properties of
    Just value -> case readInt value of
                    [(int,_)] -> int
                    _         -> defaultWorkers
    Nothing    -> defaultWorkers

--------------------------------------------------------------------------
-- Name of user property file:
propertyFileName :: IO String
propertyFileName = getHomeDirectory >>= return . (</> ".curryanalysisrc")

defaultPropertyFileName :: String
defaultPropertyFileName = baseDir </> "curryanalysisrc"

--- Install user property file if it does not exist.
installPropertyFile :: IO ()
installPropertyFile = do
  fname <- propertyFileName
  pfexists <- doesFileExist fname
  unless pfexists $ do
    copyFile defaultPropertyFileName fname
    putStrLn $ "New analysis configuration file '" ++ fname ++ "' installed."

--- Reads the rc file (and try to install a user copy of it if it does not
--- exist) and returns its definition. Additionally, the definitions
--- are compared with the default property file of the CASS distribution.
--- If the set of variables is different, the rc file of the user is updated
--- with the distribution but the user's definitions are kept.
readRCFile :: IO CConfig
readRCFile = do
  hashomedir <- getHomeDirectory >>= doesDirectoryExist
  if not hashomedir
   then readPropertiesAndStoreLocally
   else do
     installPropertyFile
     CConfig userprops dl <- readPropertiesAndStoreLocally
     distprops <- readPropertyFile defaultPropertyFileName
     unless (rcKeys userprops == rcKeys distprops) $ do
       rcName <- propertyFileName
       putStrLn $ "Updating '" ++ rcName ++ "'..."
       renameFile rcName $ rcName <.> "bak"
       copyFile defaultPropertyFileName rcName
       mapM_ (\ (n, v) -> maybe (return ())
                 (\uv -> if uv == v then return ()
                                    else updatePropertyFile rcName n uv)
                 (lookup n userprops))
             distprops
     return (CConfig userprops dl)

rcKeys :: [(String, String)] -> [String]
rcKeys = sort . map fst

--- Reads the user property file or, if it does not exist,
--- the default property file of CASS,
--- and store the properties in a global variable for next access.
readPropertiesAndStoreLocally :: IO CConfig
readPropertiesAndStoreLocally = do
  userpfn    <- propertyFileName
  hasuserpfn <- doesFileExist userpfn
  props      <- readPropertyFile
                  (if hasuserpfn then userpfn else defaultPropertyFileName)
  return $ updateDebugLevel (CConfig props Quiet)

--- Updates the debug level from the current properties.
updateDebugLevel :: CConfig -> CConfig
updateDebugLevel cc@(CConfig properties _) =
  case lookup "debugLevel" properties of
    Just value -> case readInt value of
                    [(dl,_)] -> setDebugLevel dl cc
                    _        -> cc
    Nothing    -> cc

-- Updates a property.
updateProperty :: String -> String -> CConfig -> CConfig
updateProperty pn pv (CConfig currprops dl) =
  let newprops = replaceKeyValue pn pv currprops
  in updateDebugLevel (CConfig newprops dl)

replaceKeyValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replaceKeyValue k v []            = [(k,v)]
replaceKeyValue k v ((k1,v1):kvs) =
  if k == k1 then (k,v) : kvs
             else (k1,v1) : replaceKeyValue k v kvs


--------------------------------------------------------------------------
--- Gets the name of file containing the current server port and pid
--- ($HOME has to be set)
getServerPortFileName :: IO String
getServerPortFileName = do
  homeDir <- getHomeDirectory
  return $ homeDir++"/.curryanalysis.port"

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
