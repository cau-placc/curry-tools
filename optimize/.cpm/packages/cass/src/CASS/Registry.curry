--------------------------------------------------------------------
--- This module collects all analyses in the analysis system.
---
--- Each analysis available in the analysis system must be
--- registered in the top part of this module.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version January 2017
--------------------------------------------------------------------

module CASS.Registry
 ( functionAnalysisInfos, registeredAnalysisNames, registeredAnalysisInfos
 , lookupRegAnaWorker, runAnalysisWithWorkers, analyzeMain
 ) where

import FlatCurry.Types
import FlatCurry.Goodies(progImports)
import IO
import IOExts
import XML

import Analysis.Logging (debugMessage)
import Analysis.Files (getImports, loadCompleteAnalysis)
import Analysis.ProgInfo
import Analysis.Types
import CASS.Configuration(numberOfWorkers)
import CASS.Dependencies(getModulesToAnalyze)
import CASS.ServerFunctions(masterLoop)
import CASS.WorkerFunctions(analysisClient)

--------------------------------------------------------------------
-- Configurable part of this module.
--------------------------------------------------------------------

import Analysis.Demandedness
import Analysis.Deterministic
import Analysis.Groundness
import Analysis.HigherOrder
import Analysis.Indeterministic
import Analysis.RequiredValue
import qualified Analysis.RequiredValues as RVS
import Analysis.RightLinearity
import Analysis.RootReplaced
import Analysis.SensibleTypes
import Analysis.SolutionCompleteness
import Analysis.Termination
import Analysis.TotallyDefined
import Analysis.TypeUsage
import Analysis.UnsafeModule

--------------------------------------------------------------------
--- Each analysis used in our tool must be registered in this list
--- together with an operation to show the analysis result as a string.
registeredAnalysis :: [RegisteredAnalysis]
registeredAnalysis =
  [cassAnalysis "Functionally defined"       functionalAnalysis showFunctional
  ,cassAnalysis "Overlapping rules"          overlapAnalysis  showOverlap
  ,cassAnalysis "Deterministic operations"   nondetAnalysis   showDet
  ,cassAnalysis "Depends on non-deterministic operations"
                                             nondetDepAnalysis showNonDetDeps
  ,cassAnalysis "Depends on all non-deterministic operations"
                                             nondetDepAllAnalysis showNonDetDeps
  ,cassAnalysis "Right-linear operations"    rlinAnalysis     showRightLinear
  ,cassAnalysis "Solution completeness"      solcompAnalysis  showSolComplete
  ,cassAnalysis "Pattern completeness"       patCompAnalysis  showComplete
  ,cassAnalysis "Totally defined operations" totalAnalysis    showTotally
  ,cassAnalysis "Indeterministic operations" indetAnalysis    showIndet
  ,cassAnalysis "Demanded arguments"         demandAnalysis   showDemand
  ,cassAnalysis "Groundness"                 groundAnalysis   showGround
  ,cassAnalysis "Non-determinism effects"    ndEffectAnalysis showNDEffect
  ,cassAnalysis "Higher-order datatypes"     hiOrdType        showOrder
  ,cassAnalysis "Higher-order constructors"  hiOrdCons        showOrder
  ,cassAnalysis "Higher-order functions"     hiOrdFunc        showOrder
  ,cassAnalysis "Productive operations"    productivityAnalysis showProductivity
  ,cassAnalysis "Sensible types"             sensibleType     showSensible
  ,cassAnalysis "Sibling constructors"       siblingCons      showSibling
  ,cassAnalysis "Required value"             reqValueAnalysis showAFType
  ,cassAnalysis "Required value sets"        RVS.reqValueAnalysis RVS.showAFType
  ,cassAnalysis "Root cyclic replacements"   rootCyclicAnalysis showRootCyclic
  ,cassAnalysis "Root replacements"          rootReplAnalysis showRootRepl
  ,cassAnalysis "Terminating operations"     terminationAnalysis showTermination
  ,cassAnalysis "Types in values"            typesInValuesAnalysis showTypeNames
  ,cassAnalysis "Unsafe module"              unsafeModuleAnalysis  showUnsafe
  ]


--------------------------------------------------------------------
-- Static part of this module follows below
--------------------------------------------------------------------

--- This auxiliary operation creates a new program analysis to be used
--- by the server/client analysis tool from a given analysis and
--- analysis show function. The first argument is a short title for the
--- analysis.
cassAnalysis :: Eq a => String -> Analysis a -> (AOutFormat -> a -> String)
             -> RegisteredAnalysis
cassAnalysis title analysis showres =
  RegAna (analysisName analysis)
         (isFunctionAnalysis analysis)
         title
         (analyzeAsString analysis showres)
         (analysisClient analysis)

--- The type of all registered analysis.
--- The components are as follows:
--- * the name of the analysis
--- * is this a function analysis?
--- * a long meaningful title of the analysis
--- * the operation used by the server to distribute analysis work
---   to the clients
--- * the worker operation to analyze a list of modules
data RegisteredAnalysis =
  RegAna String
         Bool
         String
         (String -> Bool -> [Handle] -> Maybe AOutFormat
                 -> IO (Either (ProgInfo String) String))
         ([String] -> IO ())

regAnaName :: RegisteredAnalysis -> String
regAnaName (RegAna n _ _ _ _) = n

regAnaInfo :: RegisteredAnalysis -> (String,String)
regAnaInfo (RegAna n _ t _ _) = (n,t)

regAnaFunc :: RegisteredAnalysis -> Bool
regAnaFunc (RegAna _ fa _ _ _) = fa

regAnaServer :: RegisteredAnalysis
                -> (String -> Bool -> [Handle] -> Maybe AOutFormat
                    -> IO (Either (ProgInfo String) String))
regAnaServer (RegAna _ _ _ a _) = a

regAnaWorker :: RegisteredAnalysis -> ([String] -> IO ())
regAnaWorker (RegAna _ _ _ _ a) = a

--- Names of all registered analyses.
registeredAnalysisNames :: [String]
registeredAnalysisNames = map regAnaName registeredAnalysis

--- Names and titles of all registered analyses.
registeredAnalysisInfos :: [(String,String)]
registeredAnalysisInfos = map regAnaInfo registeredAnalysis

--- Names and titles of all registered function analyses.
functionAnalysisInfos :: [(String,String)]
functionAnalysisInfos = map regAnaInfo (filter regAnaFunc registeredAnalysis)

lookupRegAna :: String -> [RegisteredAnalysis] -> Maybe RegisteredAnalysis
lookupRegAna _ [] = Nothing
lookupRegAna aname (ra@(RegAna raname _ _ _ _) : ras) =
  if aname==raname then Just ra else lookupRegAna aname ras

-- Look up a registered analysis server with a given analysis name.
lookupRegAnaServer :: String -> (String -> Bool -> [Handle] -> Maybe AOutFormat
                                        -> IO (Either (ProgInfo String) String))
lookupRegAnaServer aname =
  maybe (\_ _ _ _ -> return (Right ("unknown analysis: "++aname)))
        regAnaServer
        (lookupRegAna aname registeredAnalysis)

-- Look up a registered analysis worker with a given analysis name.
lookupRegAnaWorker :: String -> ([String] -> IO ())
lookupRegAnaWorker aname =
  maybe (const done) regAnaWorker (lookupRegAna aname registeredAnalysis)

--------------------------------------------------------------------
-- Run an analysis with a given name on a given module with a list
-- of workers identified by their handles and return the analysis results.
runAnalysisWithWorkers :: String -> AOutFormat -> Bool -> [Handle] -> String
                       -> IO (Either (ProgInfo String) String)
runAnalysisWithWorkers ananame aoutformat enforce handles moduleName =
  (lookupRegAnaServer ananame) moduleName enforce handles (Just aoutformat)

-- Run an analysis with a given name on a given module with a list
-- of workers identified by their handles but do not load analysis results.
runAnalysisWithWorkersNoLoad :: String -> [Handle] -> String -> IO ()
runAnalysisWithWorkersNoLoad ananame handles moduleName =
  (lookupRegAnaServer ananame) moduleName False handles Nothing >> done

--- Generic operation to analyze a module.
--- The parameters are the analysis, the show operation for analysis results,
--- the name of the main module to be analyzed,
--- a flag indicating whether the (re-)analysis should be enforced,
--- the handles for the workers,
--- and a flag indicating whether the analysis results should be loaded
--- and returned (if the flag is false, the result contains the empty
--- program information).
--- An error occurred during the analysis is returned as `(Right ...)`.
analyzeAsString :: Analysis a -> (AOutFormat->a->String) -> String -> Bool
                -> [Handle] -> Maybe AOutFormat
                -> IO (Either (ProgInfo String) String)
analyzeAsString analysis showres modname enforce handles mbaoutformat = do
  analyzeMain analysis modname handles enforce (mbaoutformat /= Nothing) >>=
    return . either (Left . mapProgInfo (showres aoutformat)) Right
 where
  aoutformat = maybe AText id mbaoutformat

--- Generic operation to analyze a module.
--- The parameters are the analysis, the name of the main module
--- to be analyzed, the handles for the workers,
--- a flag indicating whether the (re-)analysis should be enforced,
--- and a flag indicating whether the analysis results should be loaded
--- and returned (if the flag is false, the result contains the empty
--- program information).
--- An error occurred during the analysis is returned as `(Right ...)`.
analyzeMain :: Analysis a -> String -> [Handle] -> Bool -> Bool
            -> IO (Either (ProgInfo a) String)
analyzeMain analysis modname handles enforce load = do
  let ananame = analysisName analysis
  debugMessage 2 ("Start analysis: "++modname++"/"++ananame)
  modulesToDo <- getModulesToAnalyze enforce analysis modname
  let numModules = length modulesToDo
  workresult <-
    if numModules==0
    then return Nothing
    else do
     when (numModules>1) $
       debugMessage 1
         ("Number of modules to be analyzed: " ++ show numModules)
     prepareCombinedAnalysis analysis modname (map fst modulesToDo) handles
     numworkers <- numberOfWorkers
     if numworkers>0
       then do debugMessage 2 "Starting master loop"
               masterLoop handles [] ananame modname modulesToDo []
       else analyzeLocally ananame (map fst modulesToDo)
  result <-
    maybe (if load
           then do debugMessage 3 ("Reading analysis of: "++modname)
                   loadCompleteAnalysis ananame modname >>= return . Left
           else return (Left emptyProgInfo))
          (return . Right)
          workresult
  debugMessage 4 ("Result: " ++ either showProgInfo id result)
  return result

-- Analyze a module and all its imports locally without worker processes.
analyzeLocally :: String -> [String] -> IO (Maybe String)
analyzeLocally ananame modules = do
  debugMessage 3 ("Local analysis of: "++ananame++"/"++show modules)
  (lookupRegAnaWorker ananame) modules -- run client
  return Nothing


-- Perform the first analysis part of a combined analysis
-- so that their results are available for the main analysis.
prepareCombinedAnalysis:: Analysis a -> String -> [String] -> [Handle] -> IO ()
prepareCombinedAnalysis analysis moduleName depmods handles =
  if isCombinedAnalysis analysis
  then
    if isSimpleAnalysis analysis
    then do
      -- the directly imported interface information might be required...
      importedModules <- getImports moduleName
      mapIO_ (\basename ->
                mapIO_ (runAnalysisWithWorkersNoLoad basename handles)
                       (importedModules++[moduleName]))
             baseAnaNames
    else
      -- for a dependency analysis, the information of all implicitly
      -- imported modules might be required:
      mapIO_ (\baseaname ->
                mapIO_ (runAnalysisWithWorkersNoLoad baseaname handles) depmods)
             baseAnaNames
  else done
 where
   baseAnaNames = baseAnalysisNames analysis

--------------------------------------------------------------------
