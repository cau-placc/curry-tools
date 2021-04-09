--------------------------------------------------------------------------
--- This module supports the configuration of the analysis system
--- and provides access to some values in Config file.
---
--- It also provides an operation to get the port number of
--- the analysis server (which is implicitly started if necessary).
---
--- @author Michael Hanus
--- @version February 2021
--------------------------------------------------------------------------

module CASS.Configuration
 ( systemBanner, baseDir, docDir, executableName
 , getServerAddress, updateRCFile, updateCurrentProperty
 , getFPMethod, getWithPrelude
 , storeServerPortNumber, removeServerPortNumber, getServerPortNumber
 , getDefaultPath, waitTime, numberOfWorkers
 ) where

import Curry.Compiler.Distribution ( curryCompiler )
import Data.List           ( sort )
import Numeric             ( readInt )
import System.Environment  ( getEnv )
import System.FilePath     ( FilePath, (</>), (<.>) )

import System.Process
import System.Directory
import Global

import Analysis.Logging   ( debugMessage, setDebugLevel )
import CASS.PackageConfig ( packagePath, packageExecutable, packageVersion )
import Data.PropertyFile  ( readPropertyFile, updatePropertyFile )

systemBanner :: String
systemBanner =
  let bannerText = "CASS: Curry Analysis Server System (Version " ++
                   packageVersion ++ " of 04/02/2021 for " ++
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
  if pfexists then return () else do
    copyFile defaultPropertyFileName fname
    putStrLn ("New analysis configuration file '"++fname++"' installed.")

--- Reads the rc file (and try to install a user copy of it if it does not
--- exist) and compares the definitions with the default property file
--- of the CASS distribution. If the set of variables is different,
--- update the rc file of the user with the distribution
--- but keep the user's definitions.
updateRCFile :: IO ()
updateRCFile = do
  hashomedir <- getHomeDirectory >>= doesDirectoryExist
  if not hashomedir
   then readPropertiesAndStoreLocally >> return ()
   else do
     installPropertyFile
     userprops <- readPropertiesAndStoreLocally
     distprops <- readPropertyFile defaultPropertyFileName
     if (rcKeys userprops == rcKeys distprops) then return () else do
       rcName    <- propertyFileName
       putStrLn $ "Updating \"" ++ rcName ++ "\"..."
       renameFile rcName $ rcName <.> "bak"
       copyFile defaultPropertyFileName rcName
       mapM_ (\ (n, v) -> maybe (return ())
                 (\uv -> if uv==v then return () else updatePropertyFile rcName n uv)
                 (lookup n userprops))
              distprops

rcKeys :: [(String, String)] -> [String]
rcKeys = sort . map fst

--- Reads the user property file or, if it does not exist,
--- the default property file of CASS,
--- and store the properties in a global variable for next access.
readPropertiesAndStoreLocally :: IO [(String,String)]
readPropertiesAndStoreLocally = do
  userpfn    <- propertyFileName
  hasuserpfn <- doesFileExist userpfn
  props      <- readPropertyFile
                   (if hasuserpfn then userpfn else defaultPropertyFileName)
  writeGlobal currProps (Just props)
  updateDebugLevel props
  return props

--- Reads the user property file (which must be installed!)
--- and store the properties in a global variable for next access.
getProperties :: IO [(String,String)]
getProperties =
  readGlobal currProps >>= maybe readPropertiesAndStoreLocally return

--- Updates the debug level from the current properties.
updateDebugLevel :: [(String,String)] -> IO ()
updateDebugLevel properties = do
  let number = lookup "debugLevel" properties
  case number of
    Just value -> do
      case readInt value of
        [(dl,_)] -> setDebugLevel dl
        _        -> return ()
    Nothing -> return ()

--- Global variable to store the current properties.
currProps :: Global (Maybe [(String,String)])
currProps = global Nothing Temporary

-- Updates a current property.
updateCurrentProperty :: String -> String -> IO ()
updateCurrentProperty pn pv = do
  currprops <- getProperties
  let newprops = replaceKeyValue pn pv currprops
  writeGlobal currProps (Just newprops)
  updateDebugLevel newprops

replaceKeyValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replaceKeyValue k v [] = [(k,v)]
replaceKeyValue k v ((k1,v1):kvs) =
  if k==k1 then (k,v):kvs else (k1,v1) : replaceKeyValue k v kvs


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

--- Reads the current server port number. If the server is not running,
--- it is also started.
getServerPortNumber :: IO Int
getServerPortNumber = do
  serverPortFileName <- getServerPortFileName
  exfile <- doesFileExist serverPortFileName
  if exfile
   then do (portnum,pid) <- readServerPortPid
           flag <- system ("ps -p "++show pid++" > /dev/null")
           if flag==0
            then return portnum
            else do removeFile serverPortFileName
                    getServerPortNumber
   else do debugMessage 2 "Starting analysis server..."
           tcmd <- getTerminalCommand
           let serverCmd = baseDir++"/cass"
           if all isSpace tcmd
            then system ("\""++serverCmd++"\"  > /dev/null 2>&1 &")
            else system (tcmd++" \""++baseDir++"/cass\" &")
           sleep 1
           waitForServerPort serverPortFileName
 where
  waitForServerPort serverPortFileName = do
    exfile <- doesFileExist serverPortFileName
    if exfile
     then readServerPortPid >>= return . fst
     else do debugMessage 2 "Waiting for server start..."
             sleep 1
             waitForServerPort serverPortFileName

--------------------------------------------------------------------------
-- Get terminalCommand from Config file
getTerminalCommand :: IO String
getTerminalCommand = do
  properties <- getProperties
  let tcmd = lookup "terminalCommand" properties
  return (maybe "" id tcmd)

-- Get the fixpoint computation method from Config file
getFPMethod :: IO String
getFPMethod =
  getProperties >>= return . maybe "simple" id . lookup "fixpoint"

-- Get the option to analyze also the prelude from Config file
getWithPrelude :: IO String
getWithPrelude =
  getProperties >>= return . maybe "yes" id . lookup "prelude"

-- timeout for network message passing: -1 is wait time infinity
waitTime :: Int
waitTime = -1

-- Default number of workers (if the number is not found in the
-- configuration file).
defaultWorkers :: Int
defaultWorkers=0

--- Gets the default load path from the property file (added at the end
--- of CURRYPATH).
getDefaultPath :: IO String
getDefaultPath = do
  currypath <- getEnv "CURRYPATH"
  properties <- getProperties
  let proppath = lookup "path" properties
  return $ case proppath of
    Just value -> if all isSpace value then currypath else
                  if null currypath then value else currypath++':':value
    Nothing -> currypath

-- number of worker threads running at the same time
numberOfWorkers :: IO Int
numberOfWorkers = do
  properties <- getProperties
  let number = lookup "numberOfWorkers" properties
  case number of
    Just value -> do
      case readInt value of
        [(int,_)] -> return int
        _         -> return defaultWorkers
    Nothing -> return defaultWorkers
