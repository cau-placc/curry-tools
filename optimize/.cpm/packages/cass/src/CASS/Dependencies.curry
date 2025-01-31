-----------------------------------------------------------------------
--- Operations to handle dependencies of analysis files.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version April 2021
-----------------------------------------------------------------------

module CASS.Dependencies(getModulesToAnalyze,reduceDependencies) where

import Control.Monad     ( when )
import FlatCurry.Types
import FlatCurry.Goodies ( progImports )
import System.Directory  ( doesFileExist, getModificationTime, removeFile )
import Data.Maybe        ( fromMaybe )
import Data.List         ( delete )
import Data.Time(ClockTime)

import RW.Base

import Analysis.Logging   ( DLevel, debugMessage )
import Analysis.Types
import Analysis.ProgInfo
import Analysis.Files
import CASS.Configuration ( CConfig, debugLevel, withPrelude, ccOptions )
import CASS.Options

-----------------------------------------------------------------------
--- Compute the modules and their imports which must be analyzed
--- w.r.t. a given analysis and main module.
--- If the first argument is true, then the analysis is enforced
--- (even if analysis information exists).
getModulesToAnalyze :: (Eq a, Read a, ReadWrite a) => CConfig -> Bool
                    -> Analysis a -> String -> IO [(String,[String])]
getModulesToAnalyze cconfig enforce analysis moduleName = do
  checkPrivateProgInfo cconfig analysis moduleName
  if isSimpleAnalysis analysis
    then do
      ananewer <- isAnalysisFileNewer ananame moduleName
      return (if ananewer && not enforce then [] else [(moduleName,[])])
    else do
      valid <- isAnalysisValid ananame moduleName
      if valid && not enforce
        then do
          debugMessage dl 3 $
            "Analysis file for '" ++ moduleName ++ "' up-to-date"
          return []
        else do
          moduleList <- getDependencyList cconfig [moduleName] []
          debugMessage dl 3 $ "Complete module list: "++ show moduleList
          let impmods = map fst moduleList
          storeImportModuleList dl moduleName impmods
          sourceTimeList <- mapM getSourceFileTime        impmods
          fcyTimeList    <- mapM getFlatCurryFileTime     impmods
          anaTimeList    <- mapM (getAnaFileTime ananame) impmods
          let (modulesToDo,modulesUpToDate) =
                  findModulesToAnalyze moduleList anaTimeList sourceTimeList
                                       fcyTimeList ([],[])
          --debugMessage dl 3 ("Modules up-to-date: "++ show modulesUpToDate)
          let modulesToAnalyze =
                if enforce
                  then moduleList
                  else
                    if withPrelude cconfig
                      then reduceDependencies modulesToDo modulesUpToDate
                      else let reduced = reduceDependencies modulesToDo
                                          (modulesUpToDate ++ ["Prelude"])
                          in case reduced of
                               (("Prelude",_):remaining) -> remaining
                               _                         -> reduced
          debugMessage dl 3 ("Modules to analyze: " ++ show modulesToAnalyze)
          return modulesToAnalyze
 where
   dl = debugLevel cconfig
   ananame = analysisName analysis

---- The empty program information for a given analysis.
emptyAnalysisInfo:: Analysis a -> ProgInfo a
emptyAnalysisInfo _ = emptyProgInfo

-- Checks whether the private `ProgInfo` is empty if option `--all` is set.
-- If this is the case, the analysis files will be deleted so that the
-- module will be re-analyzed. This is necessary if the analysis files
-- have been created from the CurryInfo system (which contains only information
-- about public entities).
checkPrivateProgInfo :: (Eq a, Read a, ReadWrite a) => CConfig
                     -> Analysis a -> String -> IO ()
checkPrivateProgInfo cconfig analysis modname
  | not (optAll (ccOptions cconfig)) = return ()
  | otherwise
  = do
  privfname <- getAnalysisPrivateFile modname ananame
  privexists <- doesFileExist privfname
  if privexists
    then do
      privinfo <- readAnalysisPrivateFile dl privfname
      when (equalProgInfo (emptyAnalysisInfo analysis) privinfo) $ do
        removeFile privfname
        removePubInfo
    else removePubInfo
 where
  dl = debugLevel cconfig
  ananame = analysisName analysis
  removePubInfo = do
    pubfname  <- getAnalysisPublicFile modname ananame
    debugMessage dl 3 $ "Removing public analysis file '" ++ pubfname ++ "'..."
    pubexists <- doesFileExist pubfname
    when pubexists $ removeFile pubfname

-- Checks whether the analysis file is up-to-date.
-- Returns True if the analysis file is newer than the source file
-- and the FlatCurry file (if is exists).
isAnalysisFileNewer :: String -> String -> IO Bool
isAnalysisFileNewer ananame modname = do
  atime <- getAnaFileTime ananame modname
  stime <- getSourceFileTime modname
  ftime <- getFlatCurryFileTime modname
  return (isAnalysisFileTimeNewer (snd atime) (Just (snd stime)) (snd ftime))

-- Is the analysis file time up-to-date w.r.t. the file times of
-- the source file and the FlatCurry file?
-- Returns True if the analysis file is newer than the source file
-- and the FlatCurry file (if is exists).
isAnalysisFileTimeNewer :: Maybe ClockTime -> Maybe ClockTime -> Maybe ClockTime
                        -> Bool
isAnalysisFileTimeNewer anatime srctime fcytime =
  anatime >= srctime && anatime >= fcytime

-- Read current import dependencies and checks whether the current analysis
-- file is valid, i.e., it is newer than the source and FlatCurry files
-- of all (directly and indirectly) imported modules.
isAnalysisValid :: String -> String -> IO Bool
isAnalysisValid ananame modname =
  getImportModuleListFile modname >>= maybe
    (return False)
    (\importListFile -> do
      itime <- getModificationTime importListFile
      stime <- getSourceFileTime modname >>= return . snd
      if itime>=stime
       then do
        implist <- readFile importListFile >>= return . read
        sourceTimeList <- mapM getSourceFileTime        implist
        fcyTimeList    <- mapM getFlatCurryFileTime     implist
        anaTimeList    <- mapM (getAnaFileTime ananame) implist
        return (all (\ (x,y,z) -> isAnalysisFileTimeNewer x y z)
                    (zip3 (map snd anaTimeList)
                          (map (Just . snd) sourceTimeList)
                          (map snd fcyTimeList)))
       else return False)


--- Gets the list of all modules required by the first module.
--- The result is sorted according to their dependencies
--- (Prelude first, main module last)
getDependencyList :: CConfig -> [String] -> [(String,[String])]
                  -> IO [(String,[String])]
getDependencyList _  []           moddeps = return moddeps
getDependencyList cc (mname:mods) moddeps =
  maybe (do --debugMessage 3 ("Getting imports of "++ mname)
            --debugMessage 3 ("Still to do: "++ show mods)
            imports <- getImports dl mname
            getDependencyList cc (addNewMods mods imports)
                              ((mname,imports):moddeps))
        (\ (newmoddeps,imps) ->
              getDependencyList cc (addNewMods mods imps) newmoddeps)
        (lookupAndReorder mname [] moddeps)
 where dl = debugLevel cc

-- add new modules if they are not already there:
addNewMods :: [String] -> [String] -> [String]
addNewMods oldmods newmods = oldmods ++ filter (`notElem` oldmods) newmods

lookupAndReorder :: String -> [(String, [String])] -> [(String, [String])]
                 -> Maybe ([(String, [String])], [String])
lookupAndReorder _ _ [] = Nothing
lookupAndReorder mname list1 ((amod,amodimports):rest)
  | mname==amod = Just ((amod,amodimports):reverse list1++rest, amodimports)
  | otherwise   = lookupAndReorder mname ((amod,amodimports):list1) rest

-- get timestamp of analysis file
getAnaFileTime :: String -> String -> IO (String,Maybe ClockTime)
getAnaFileTime anaName moduleName = do
  fileName <- getAnalysisPublicFile moduleName anaName
  fileExists <- doesFileExist fileName
  if fileExists
    then do time <- getModificationTime fileName
            return (moduleName,Just time)
    else return (moduleName,Nothing)


-- check if analysis result of a module can be loaded or needs to be
-- newly analyzed
findModulesToAnalyze :: [(String,[String])]
                     -> [(String,Maybe ClockTime)]
                     -> [(String,ClockTime)]
                     -> [(String,Maybe ClockTime)]
                     -> ([(String,[String])],[String])
                     -> ([(String,[String])],[String])
findModulesToAnalyze [] _ _ _ (modulesToDo,modulesUpToDate) =
  (reverse modulesToDo, modulesUpToDate)
findModulesToAnalyze (m@(mod,imports):ms)
                     anaTimeList sourceTimeList fcyTimeList
                     (modulesToDo,modulesUpToDate) =
  case (lookup mod anaTimeList) of
    Just Nothing    -> findModulesToAnalyze ms anaTimeList sourceTimeList
                                            fcyTimeList
                                            ((m:modulesToDo),modulesUpToDate)
    Just (Just time) ->
      if checkTime mod time imports anaTimeList sourceTimeList fcyTimeList
                   modulesToDo
      then findModulesToAnalyze ms anaTimeList sourceTimeList fcyTimeList
                                (modulesToDo,(mod:modulesUpToDate))
      else findModulesToAnalyze ms anaTimeList sourceTimeList fcyTimeList
                                ((m:modulesToDo),modulesUpToDate)
    Nothing -> error
                 "Internal error in AnalysisDependencies.findModulesToAnalyz"


-- function to check if result file is up-to-date
-- compares timestamp of analysis result file with module source/FlatCurry file
-- and with timpestamp of result files of all imported modules
checkTime :: String -> ClockTime -> [String] -> [(String,Maybe ClockTime)]
          -> [(String,ClockTime)] -> [(String,Maybe ClockTime)]
          -> [(String,[String])] -> Bool
checkTime mod time1 [] _ sourceTimeList fcyTimeList _ =
  isAnalysisFileTimeNewer (Just time1) (lookup mod sourceTimeList)
                          (fromMaybe Nothing (lookup mod fcyTimeList))
checkTime mod time1 (impt:impts) anaTimeList sourceTimeList fcyTimeList
          resultList =
  (lookup impt resultList) == Nothing
  && (Just time1) >= (fromMaybe Nothing (lookup impt anaTimeList))
  && checkTime mod time1 impts anaTimeList sourceTimeList fcyTimeList resultList

-----------------------------------------------------------------------
-- Remove the module analysis dependencies (first argument) w.r.t.
-- a list of modules that are already analyzed (second argument).
reduceDependencies :: [(String,[String])] -> [String] -> [(String,[String])]
reduceDependencies modulesToDo [] = modulesToDo
reduceDependencies modulesToDo (mod:mods) =
  let modulesToDo2 = map (\ (m,list) -> (m,(delete mod list))) modulesToDo
   in reduceDependencies modulesToDo2 mods
