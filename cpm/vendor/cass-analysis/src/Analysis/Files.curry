--------------------------------------------------------------------------
--- This module contains operations to load and store analysis information
--- persistently in files.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version March 2017
--------------------------------------------------------------------------

module Analysis.Files where

import Directory
import Distribution      ( installDir, lookupModuleSourceInLoadPath
                         , stripCurrySuffix )
import FlatCurry.Files
import FlatCurry.Goodies (progImports)
import FlatCurry.Types   (Prog, QName)
import FilePath
import List              (isPrefixOf, isSuffixOf)
import ReadShowTerm      (readQTerm, showQTerm)
import Time              (ClockTime)

import Analysis.Logging  (debugMessage)
import Analysis.ProgInfo


--- Get the file name in which analysis results are stored
--- (without suffix ".pub" or ".priv")
getAnalysisBaseFile :: String -> String -> IO String
getAnalysisBaseFile moduleName anaName = do
  analysisDirectory <- getAnalysisDirectory
  currentDir        <- getCurrentDirectory >>= return . dropDrive
  let modAnaName = moduleName <.> anaName
  (fileDir,_) <- findModuleSourceInLoadPath moduleName
  if isAbsolute fileDir
   then return (analysisDirectory </> dropDrive fileDir </> modAnaName)
   else return (analysisDirectory </> currentDir </> fileDir </> modAnaName)

--- Get the file name in which public analysis results are stored.
getAnalysisPublicFile :: String -> String -> IO String
getAnalysisPublicFile modname ananame = do
  getAnalysisBaseFile modname ananame >>= return . (<.> "pub")

-- Cache directory where analysis info files are stored.
-- If $HOME exists, it is ~/.curryanalysis_cache
getAnalysisDirectory :: IO String
getAnalysisDirectory = do
  homedir <- getHomeDirectory
  hashomedir <- doesDirectoryExist homedir
  let cassStoreDir = if hashomedir then homedir else installDir
  return $ cassStoreDir </> ".curryanalysis_cache"

-- loads analysis results for a list of modules
getInterfaceInfos :: String -> [String] -> IO (ProgInfo a)
getInterfaceInfos _ [] = return emptyProgInfo
getInterfaceInfos anaName (mod:mods) =
  do modInfo  <- loadPublicAnalysis anaName mod 
     modsInfo <- getInterfaceInfos anaName mods
     return (combineProgInfo modInfo modsInfo)

--- Gets the file name in which default analysis values different from
--- standard start values are stored. Typically, such a file contains
--- specific analysis information for external operations.
--- The file must contain a term of the type `[(String,a)]` where
--- the first component of each pair is the name of the operation
--- (it is assumed that this denotes an operation of the current module)
--- and the second component is an analysis value.
loadDefaultAnalysisValues :: String -> String -> IO [(QName,a)]
loadDefaultAnalysisValues anaName moduleName = do
  (_,fileName) <- findModuleSourceInLoadPath moduleName
  let defaultFileName = stripCurrySuffix fileName ++ ".defaults." ++ anaName
  fileExists <- doesFileExist defaultFileName
  if fileExists
    then do debugMessage 3 ("Load default values from " ++ defaultFileName)
            defaultValues <- readFile defaultFileName >>= return . readQTerm
            return (map (\ (f,a) -> ((moduleName,f),a)) defaultValues)
    else return []

--- Loads the currently stored analysis information for a module.
loadCompleteAnalysis :: String -> String -> IO (ProgInfo _)
loadCompleteAnalysis ananame mainModule =
  getAnalysisBaseFile mainModule ananame >>= readAnalysisFiles

--- Reads analysis result from file for the public entities of a given module.
loadPublicAnalysis:: String -> String -> IO (ProgInfo a) 
loadPublicAnalysis anaName moduleName = do
  getAnalysisPublicFile moduleName anaName >>= readAnalysisPublicFile

--- Store current import dependencies.
storeImportModuleList :: String -> [String] -> IO ()
storeImportModuleList modname modlist = do
  importListFile <- getAnalysisBaseFile modname "IMPORTLIST"
  createDirectoryR (dropFileName importListFile)
  writeFile importListFile (showQTerm modlist)

--- Gets the file containing import dependencies for a main module
--- (if it exists).
getImportModuleListFile :: String -> IO (Maybe String)
getImportModuleListFile modname = do
  importListFile <- getAnalysisBaseFile modname "IMPORTLIST"
  iflExists <- doesFileExist importListFile
  return $ if iflExists then Just importListFile else Nothing

--- Store an analysis results in a file and create directories if neccesssary.
--- The first argument is the analysis name.
storeAnalysisResult :: String -> String -> ProgInfo a -> IO ()
storeAnalysisResult ananame moduleName result = do
   baseFileName <- getAnalysisBaseFile moduleName ananame
   createDirectoryR (dropFileName baseFileName)
   debugMessage 4 ("Analysis result: " ++ showProgInfo result)
   writeAnalysisFiles baseFileName result

-- creates directory (and all needed root-directories) recursively
createDirectoryR :: String -> IO ()
createDirectoryR maindir = 
  let (drv,dir) = splitDrive maindir
   in createDirectories drv (splitDirectories dir)
 where
  createDirectories _ [] = done  
  createDirectories dirname (dir:dirs) = do
    let createdDir = dirname </> dir
    dirExists <- doesDirectoryExist createdDir
    unless dirExists $ do
      debugMessage 3 ("Creating directory '"++createdDir++"'...")
      createDirectory createdDir
    createDirectories createdDir dirs

--- Deletes all analysis files for a given analysis name.
deleteAllAnalysisFiles :: String -> IO ()
deleteAllAnalysisFiles ananame = do
   analysisDir <- getAnalysisDirectory
   deleteAllInDir analysisDir
 where
  deleteAllInDir dir = do
    dircont <- getDirectoryContents dir
    mapIO_ processDirElem (filter (not . isPrefixOf ".") dircont)
   where
     processDirElem f = do
       let fullname = dir </> f
       when (isAnaFile f) $ do
         putStrLn ("DELETE: " ++ fullname)
         removeFile fullname
       isdir <- doesDirectoryExist fullname
       when isdir $ deleteAllInDir fullname

     isAnaFile f =
       (".pub" `isSuffixOf` f && ('.':ananame) `isSuffixOf` dropExtension f) ||
       (".priv" `isSuffixOf` f && ('.':ananame) `isSuffixOf` dropExtension f)


--------------------------------------------------------------------------
-- Auxiliaries for dealing with Curry files.

--- Returns a directory name and the actual source file name for a module
--- by looking up the module source in the current load path.
--- If the module is hierarchical, the directory is the top directory
--- of the hierarchy.
--- An error is raised if there is no corresponding source file.
findModuleSourceInLoadPath :: String -> IO (String,String)
findModuleSourceInLoadPath modname =
  lookupModuleSourceInLoadPath modname >>=
  maybe (error $ "Source file for module '"++modname++"' not found!")
        return

--- Get the imports of a module.
getImports :: String -> IO [String]
getImports moduleName = do
  debugMessage 3 ("Reading interface of module "++moduleName)
  readNewestFlatCurryInt moduleName >>= return . progImports

-- Get timestamp of a Curry source module file (together with the module name)
getSourceFileTime :: String -> IO (String,ClockTime)
getSourceFileTime moduleName = do
  (_,fileName) <- findModuleSourceInLoadPath moduleName
  time <- getModificationTime fileName
  return (moduleName,time)

-- Get timestamp of FlatCurry file (together with the module name)
getFlatCurryFileTime :: String -> IO (String,Maybe ClockTime)
getFlatCurryFileTime modname =
  lookupFlatCurryFileInLoadPath modname >>=
  maybe (return (modname, Nothing))
        (\fcyFileName -> do
            ftime <- getModificationTime fcyFileName
            return (modname, Just ftime))

--- Returns name of the FlatCurry file of a module if this file exists
--- and is newer than the source file.
flatCurryFileNewer :: String -> IO (Maybe String)
flatCurryFileNewer modname = do
  (_,sourceFileName) <- findModuleSourceInLoadPath modname
  stime <- getModificationTime sourceFileName
  lookupFlatCurryFileInLoadPath modname >>=
   maybe (return Nothing)
         (\fcyFileName -> do
            itime <- getModificationTime fcyFileName
            return (if itime >= stime then Just fcyFileName else Nothing))

--- Returns the newest FlatCurry program for a module.
--- The source program is parsed if the interface older than the source,
--- otherwise the FlatCurry program is read without parsing
--- (note that this returns only the correct version if the
--- imported modules are already parsed or are not relevant here).
readNewestFlatCurry :: String -> IO Prog
readNewestFlatCurry modname =
  flatCurryFileNewer modname >>=
  maybe (readFlatCurry modname) readFlatCurryFile

--- Returns the newest FlatCurry interface for a module.
--- The source program is parsed if the interface older than the source,
--- otherwise the FlatCurry interface file is read without parsing
--- (note that this returns only the correct version if the
--- imported modules are already parsed or are not relevant here).
readNewestFlatCurryInt :: String -> IO Prog
readNewestFlatCurryInt modname =
  flatCurryFileNewer modname >>=
  maybe (readFlatCurryInt modname) (readFlatCurryFile . flat2intName)

--- Translates FlatCurry file name to corresponding FlatCurry interface
--- file name.
flat2intName :: String -> String
flat2intName fn = reverse ("tnif" ++ drop 3 (reverse fn))
