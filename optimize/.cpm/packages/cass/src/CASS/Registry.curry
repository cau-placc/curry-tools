--------------------------------------------------------------------
--- This module collects all analyses in the analysis system.
---
--- Each analysis available in the analysis system must be
--- registered in the top part of this module.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version October 2023
--------------------------------------------------------------------

module CASS.Registry
 ( functionAnalysisInfos, registeredAnalysisNames, registeredAnalysisInfos
 , lookupRegAnaWorker, runAnalysisWithWorkers, analyzeMain
 ) where

import FlatCurry.Types
import FlatCurry.Goodies(progImports)
import System.IO
import System.IOExts
import Control.Monad
import XML

import Analysis.Logging (debugMessage)
import Analysis.Files (getImports, loadCompleteAnalysis)
import Analysis.ProgInfo
import Analysis.Types
import CASS.Configuration   ( CConfig, debugLevel, numberOfWorkers )
import CASS.Dependencies    ( getModulesToAnalyze )
import CASS.ServerFormats   ( OutputFormat(..) )
import CASS.ServerFunctions ( masterLoop )
import CASS.WorkerFunctions ( analysisClient )

--------------------------------------------------------------------
-- Configurable part of this module.
--------------------------------------------------------------------

import Analysis.Demandedness
import Analysis.Deterministic
import Analysis.Groundness
import Analysis.HigherOrder
import Analysis.Indeterministic
import Analysis.NondetOps
import Analysis.RequiredValue
import qualified Analysis.RequiredValues as RVS
import Analysis.RightLinearity
import Analysis.Residuation
import Analysis.RootReplaced
import Analysis.SensibleTypes
import Analysis.SolutionCompleteness
import Analysis.Termination
import Analysis.TotallyDefined
import Analysis.TypeUsage
import Analysis.UnsafeModule
import Analysis.Values

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
  ,cassAnalysis "Totally and functionally defined operations"
                                             totalFuncAnalysis showTotalFunc
  ,cassAnalysis "Indeterministic operations" indetAnalysis    showIndet
  ,cassAnalysis "Demanded arguments"         demandAnalysis   showDemand
  ,cassAnalysis "Groundness"                 groundAnalysis   showGround
  ,cassAnalysis "Non-deterministic operations" nondetOperations showNondet
  ,cassAnalysis "Non-determinism effects"    ndEffectAnalysis showNDEffect
  ,cassAnalysis "Higher-order datatypes"     hiOrdType        showOrder
  ,cassAnalysis "Higher-order constructors"  hiOrdCons        showOrder
  ,cassAnalysis "Higher-order functions"     hiOrdFunc        showOrder
  ,cassAnalysis "Productive operations"    productivityAnalysis showProductivity
  ,cassAnalysis "Sensible types"             sensibleType     showSensible
  ,cassAnalysis "Sibling constructors"       siblingCons      showSibling
  ,cassAnalysis "Sibling constructors with corresponding type declaration"
                siblingConsAndDecl showSiblingAndDecl
  ,cassAnalysis "Required value"             reqValueAnalysis showAFType
  ,cassAnalysis "Required value sets"        RVS.reqValueAnalysis RVS.showAFType
  ,cassAnalysis "Result values (top constructors)"
                resultValueAnalysisTop showValue
  ,cassAnalysis "Result values (up to depth 2)" resultValueAnalysis2 showValue
  ,cassAnalysis "Result values (up to depth 5)" resultValueAnalysis5 showValue
  ,cassAnalysis "Residuating operations"     residuationAnalysis showResInfo
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
cassAnalysis :: (Read a, Show a, Eq a)
             => String -> Analysis a -> (AOutFormat -> a -> String)
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
         (CConfig -> String -> Bool -> [Handle]
                  -> OutputFormat -> Maybe AOutFormat
                  -> IO (Either (ProgInfo String) String))
         (CConfig -> [String] -> IO ())

regAnaName :: RegisteredAnalysis -> String
regAnaName (RegAna n _ _ _ _) = n

regAnaInfo :: RegisteredAnalysis -> (String,String)
regAnaInfo (RegAna n _ t _ _) = (n,t)

regAnaFunc :: RegisteredAnalysis -> Bool
regAnaFunc (RegAna _ fa _ _ _) = fa

regAnaServer :: RegisteredAnalysis
             -> (CConfig -> String -> Bool -> [Handle] -> OutputFormat
             -> Maybe AOutFormat -> IO (Either (ProgInfo String) String))
regAnaServer (RegAna _ _ _ a _) = a

regAnaWorker :: RegisteredAnalysis -> (CConfig -> [String] -> IO ())
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
lookupRegAnaServer :: String
                   -> (CConfig -> String -> Bool -> [Handle] -> OutputFormat
                   -> Maybe AOutFormat
                   -> IO (Either (ProgInfo String) String))
lookupRegAnaServer aname =
  maybe (\_ _ _ _ _ _ -> return (Right $ "unknown analysis: " ++ aname))
        regAnaServer
        (lookupRegAna aname registeredAnalysis)

-- Look up a registered analysis worker with a given analysis name.
lookupRegAnaWorker :: String -> (CConfig -> [String] -> IO ())
lookupRegAnaWorker aname =
  maybe (\_ _ -> return ())
        regAnaWorker
        (lookupRegAna aname registeredAnalysis)

--------------------------------------------------------------------
-- Run an analysis with a given name on a given module with a list
-- of workers identified by their handles and return the analysis results.
runAnalysisWithWorkers :: CConfig -> String -> OutputFormat -> AOutFormat
                       -> Bool -> [Handle]
                       -> String -> IO (Either (ProgInfo String) String)
runAnalysisWithWorkers cc ananame outformat aoutformat enforce handles
                       moduleName =
  (lookupRegAnaServer ananame)
    cc moduleName enforce handles outformat (Just aoutformat)

-- Run an analysis with a given name on a given module with a list
-- of workers identified by their handles but do not load analysis results.
runAnalysisWithWorkersNoLoad :: CConfig -> String -> [Handle] -> String -> IO ()
runAnalysisWithWorkersNoLoad cc ananame handles moduleName =
  () <$ (lookupRegAnaServer ananame)
          cc moduleName False handles FormatText Nothing

--- Generic operation to analyze a module.
--- The parameters are the analysis, the show operation for analysis results,
--- the name of the main module to be analyzed,
--- a flag indicating whether the (re-)analysis should be enforced,
--- the handles for the workers,
--- and a flag indicating whether the analysis results should be loaded
--- and returned (if the flag is false, the result contains the empty
--- program information).
--- An error occurred during the analysis is returned as `(Right ...)`.
analyzeAsString :: (Read a, Show a)
                => Analysis a -> (AOutFormat -> a -> String) -> CConfig
                -> String -> Bool -> [Handle]
                -> OutputFormat -> Maybe AOutFormat
                -> IO (Either (ProgInfo String) String)
analyzeAsString analysis showres cconfig
                modname enforce handles outformat mbaoutformat = do
  analyzeMain cconfig analysis modname handles enforce
              (mbaoutformat /= Nothing) >>=
    return . either (Left . mapProgInfo showResult) Right
 where
  showResult x = case outformat of
    FormatTerm     -> show x
    FormatJSONTerm -> show x
    _              -> showres (maybe AText id mbaoutformat) x

--- Generic operation to analyze a module.
--- The parameters are the analysis, the name of the main module
--- to be analyzed, the handles for the workers,
--- a flag indicating whether the (re-)analysis should be enforced,
--- and a flag indicating whether the analysis results should be loaded
--- and returned (if the flag is false, the result contains the empty
--- program information).
--- An error occurred during the analysis is returned as `(Right ...)`.
analyzeMain :: (Read a, Show a)
            => CConfig -> Analysis a -> String -> [Handle] -> Bool -> Bool
            -> IO (Either (ProgInfo a) String)
analyzeMain cconfig analysis modname handles enforce load = do
  let ananame = analysisName analysis
  debugMessage dl 2 ("Start analysis: " ++ modname ++ "/" ++ ananame)
  modulesToDo <- getModulesToAnalyze cconfig enforce analysis modname
  let numModules = length modulesToDo
  workresult <-
    if numModules==0
    then return Nothing
    else do
     when (numModules > 1) $
       debugMessage dl 1
         ("Number of modules to be analyzed: " ++ show numModules)
     prepareCombinedAnalysis cconfig analysis modname (map fst modulesToDo)
                             handles
     if numberOfWorkers cconfig > 0
       then do debugMessage dl 2 "Starting master loop"
               masterLoop cconfig handles [] ananame modname modulesToDo []
       else analyzeLocally cconfig ananame (map fst modulesToDo)
  result <-
    maybe (if load
           then do debugMessage dl 3 ("Reading analysis of: "++modname)
                   loadCompleteAnalysis dl ananame modname >>= return . Left
           else return (Left emptyProgInfo))
          (return . Right)
          workresult
  debugMessage dl 4 ("Result: " ++ either showProgInfo id result)
  return result
 where dl = debugLevel cconfig

-- Analyze a module and all its imports locally without worker processes.
analyzeLocally :: CConfig -> String -> [String] -> IO (Maybe String)
analyzeLocally cconfig ananame modules = do
  debugMessage dl 3 ("Local analysis of: "++ananame++"/"++show modules)
  (lookupRegAnaWorker ananame) cconfig modules -- run client
  return Nothing
 where dl = debugLevel cconfig

-- Perform the first analysis part of a combined analysis
-- so that their results are available for the main analysis.
prepareCombinedAnalysis :: CConfig -> Analysis a -> String -> [String]
                        -> [Handle] -> IO ()
prepareCombinedAnalysis cc analysis moduleName depmods handles =
  if isCombinedAnalysis analysis
  then
    if isSimpleAnalysis analysis
    then do
      -- the directly imported interface information might be required...
      importedModules <- getImports (debugLevel cc) moduleName
      mapM_ (\basename ->
                mapM_ (runAnalysisWithWorkersNoLoad cc basename handles)
                       (importedModules++[moduleName]))
             baseAnaNames
    else
      -- for a dependency analysis, the information of all implicitly
      -- imported modules might be required:
      mapM_ (\baseaname ->
              mapM_ (runAnalysisWithWorkersNoLoad cc baseaname handles) depmods)
            baseAnaNames
  else return ()
 where
  baseAnaNames = baseAnalysisNames analysis

--------------------------------------------------------------------
