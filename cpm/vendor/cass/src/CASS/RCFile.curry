------------------------------------------------------------------------------
--- Some operations to handle the CASS resource configuration file
--- that is stored in `$HOME/.cassrc`
---
--- @author  Michael Hanus
--- @version May 2025
------------------------------------------------------------------------------

module CASS.RCFile
  ( readRCFile, updateProperty )
 where

import Control.Monad     ( unless )
import Data.Either       ( rights )
import Data.List         ( sort )
import Numeric           ( readInt )
import System.IO         ( hPutStrLn, stderr )

import Analysis.Logging  ( DLevel(..), debugMessage )
import Data.PropertyFile ( readPropertyFile, updatePropertyFile )
import System.FilePath   ( FilePath, (</>), (<.>) )
import System.Directory  ( doesDirectoryExist, doesFileExist, getHomeDirectory
                         , renameFile )

import CASS.Configuration ( CConfig(..), debugLevel, getDefaultCConfig
                          , setDebugLevel )

------------------------------------------------------------------------------
--- Initial properties of the default RC template file.
defaultRCProps :: [Either String (String,String)]
defaultRCProps =
  [ Left "# Configuration file for 'cass' (Curry Analysis Server System)"
  , Left ""
  , Left "# The initial default path when the system is started:"
  , Left "# (this path is added at the end of an existing CURRYPATH value)"
  , Right ("path", "")
  , Left ""
  , Left "# The number of workers (if 0, no further processes are started):"
  , Right ("numberOfWorkers", "0")
  , Left ""
  , Left "# Use the tool `curry-info` to import existing analysis infos?"
  , Left "# no     : do not use it"
  , Left "# local  : use the local installation of `curry-info`"
  , Left "# remote : use the web service of `curry-info`"
  , Right ("curryinfo", "no")
  , Left ""
  , Left "# The method to compute the fixpoint in dependency analyses. Values:"
  , Left "# simple   : simple fixpoint iteration"
  , Left "# wlist    : fixpoint iteration with working lists"
  , Left "# wlistscc : fixpoint iteration with working lists where strongly connected"
  , Left "#            components are computed to guide the individual iterations"
  , Right ("fixpoint", "wlist")
  , Left ""
  , Left "# The command used to wrap the server when the system is started"
  , Left "# in server mode:"
  , Right ("terminalCommand", "gnome-terminal -e")
  , Left ""
  , Left "# The debugging level (between 0 and 4) to show more infos."
  , Left "# Meaning of the debug level:"
  , Left "# 0 : show nothing"
  , Left "# 1 : show worker activity, e.g., timings"
  , Left "# 2 : show server communication"
  , Left "# 3 : ...and show read/store information"
  , Left "# 4 : ...show also stored/computed analysis data"
  , Right ("debugLevel", "0")
  , Left ""
  , Left "# Should the prelude be analyzed? Usually, it should be 'yes'."
  , Left "# The value 'no' is only reasonable for experimental purposes"
  , Left "# (e.g., to test new analyses on small programs)."
  , Right ("prelude", "yes")
  ]

--- The contents of the default RC template file.
defaultRC :: String
defaultRC = unlines $
  map (either id (\ (k,v) -> k ++ "=" ++ v)) defaultRCProps

--- Location of the rc file of a user.
-- Name of user property file:
propertyFileName :: IO String
propertyFileName = (</> ".cassrc") `fmap` getHomeDirectory

--- Install user property file if it does not exist.
installPropertyFile :: IO ()
installPropertyFile = do
  fname <- propertyFileName
  pfexists <- doesFileExist fname
  unless pfexists $ do
    writeFile fname defaultRC
    hPutStrLn stderr $
      "New analysis configuration file '" ++ fname ++ "' installed."

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
     cc <- readPropertiesAndStoreLocally
     let distprops = rights defaultRCProps
         userprops = ccProps cc
     unless (rcKeys userprops == rcKeys distprops) $ do
       rcName <- propertyFileName
       debugMessage (debugLevel cc) 1 $ "Updating '" ++ rcName ++ "'..."
       renameFile rcName $ rcName <.> "bak"
       writeFile rcName defaultRC
       mapM_ (\ (n, v) -> maybe (return ())
                 (\uv -> if uv == v then return ()
                                    else updatePropertyFile rcName n uv)
                 (lookup n userprops))
             distprops
     return cc
 where
  rcKeys = sort . map fst

--- Reads the user property file or, if it does not exist,
--- the default property file of CASS,
--- and return the configuration with the properties.
readPropertiesAndStoreLocally :: IO CConfig
readPropertiesAndStoreLocally = do
  userpfn    <- propertyFileName
  hasuserpfn <- doesFileExist userpfn
  props      <- if hasuserpfn
                  then readPropertyFile userpfn
                  else return $ rights defaultRCProps
  defaultcc  <- getDefaultCConfig
  return $ updateDebugLevel (defaultcc { ccProps = props })

--- Updates the debug level from the current properties.
updateDebugLevel :: CConfig -> CConfig
updateDebugLevel cc =
  case lookup "debugLevel" (ccProps cc) of
    Just value -> case readInt value of
                    [(dl,_)] -> setDebugLevel dl cc
                    _        -> cc
    Nothing    -> cc

--- Updates a property.
updateProperty :: String -> String -> CConfig -> CConfig
updateProperty pn pv cc = cc { ccProps = replaceKeyValue pn pv (ccProps cc) }

replaceKeyValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replaceKeyValue k v []            = [(k,v)]
replaceKeyValue k v ((k1,v1):kvs) =
  if k == k1 then (k,v) : kvs
             else (k1,v1) : replaceKeyValue k v kvs

------------------------------------------------------------------------------
