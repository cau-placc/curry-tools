--------------------------------------------------------------------------
--- Operations to implement the client workers.
--- In particular, it contains some simple fixpoint computations.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version January 2019
--------------------------------------------------------------------------

module CASS.WorkerFunctions where

import IOExts
import List         ( partition )
import Maybe        ( fromJust )
import System       ( getCPUTime )

import Analysis.Files
import Analysis.Logging  ( debugMessage, debugString )
import Analysis.Types    ( Analysis(..), isSimpleAnalysis, isCombinedAnalysis
                         , analysisName, startValue)
import Analysis.ProgInfo ( ProgInfo, combineProgInfo, emptyProgInfo
                         , publicProgInfo, lookupProgInfo, lists2ProgInfo
                         , equalProgInfo, publicListFromProgInfo, showProgInfo )
import Data.FiniteMap
import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.Goodies
import Data.SCC          ( scc )
import Data.Set.RBTree as Set ( SetRBT, member, empty, insert, null )

import CASS.Configuration
import CASS.FlatCurryDependency ( callsDirectly, dependsDirectlyOnTypes )

-----------------------------------------------------------------------
-- Datatype to store already read ProgInfos for modules.

type ProgInfoStore a = [(String,ProgInfo a)]

newProgInfoStoreRef :: IO (IORef (ProgInfoStore _))
newProgInfoStoreRef = newIORef []

-----------------------------------------------------------------------
--- Analyze a list of modules (in the given order) with a given analysis.
--- The analysis results are stored in the corresponding analysis result files.
analysisClient :: Eq a => Analysis a -> [String] -> IO ()
analysisClient analysis modnames = do
  store <- newIORef []
  fpmethod <- getFPMethod
  mapIO_ (analysisClientWithStore store analysis fpmethod) modnames

analysisClientWithStore :: Eq a => IORef (ProgInfoStore a) -> Analysis a -> String
                        -> String -> IO ()
analysisClientWithStore store analysis fpmethod moduleName = do
  prog        <- readNewestFlatCurry moduleName
  withprelude <- getWithPrelude
  let progimports = progImports prog
      importList  = if withprelude=="no" then filter (/="Prelude") progimports
                                         else progimports
      ananame     = analysisName analysis
  importInfos <-
    if isSimpleAnalysis analysis
    then return emptyProgInfo
    else getInterfaceInfosWS store (analysisName analysis) importList
  debugString 1 $ "Analysis time for " ++ ananame ++ "/" ++ moduleName ++ ": "
  starttime <- getCPUTime
  startvals <- getStartValues analysis prog
  result <-
     if isCombinedAnalysis analysis
     then execCombinedAnalysis analysis prog importInfos
                                startvals moduleName fpmethod
     else runAnalysis analysis prog importInfos startvals fpmethod
  storeAnalysisResult ananame moduleName result
  stoptime <- getCPUTime
  debugMessage 1 $ show (stoptime-starttime) ++ " msecs"
  loadinfos <- readIORef store
  writeIORef store ((moduleName,publicProgInfo result):loadinfos)


-- Loads analysis results for a list of modules where already read results
-- are stored in an IORef.
getInterfaceInfosWS :: IORef (ProgInfoStore a) -> String -> [String]
                    -> IO (ProgInfo a)
getInterfaceInfosWS _ _ [] = return emptyProgInfo
getInterfaceInfosWS store anaName (mod:mods) = do
  loadinfos <- readIORef store
  modInfo <- maybe (loadAndStoreAnalysis loadinfos) return
                   (lookup mod loadinfos)
  modsInfo <- getInterfaceInfosWS store anaName mods
  return (combineProgInfo modInfo modsInfo)
 where
  loadAndStoreAnalysis loadinfos = do
    info <- loadPublicAnalysis anaName mod
    writeIORef store ((mod,info):loadinfos)
    return info


-----------------------------------------------------------------------

--- Compute the start (bottom) values for a dependency analysis.
getStartValues :: Analysis a -> Prog -> IO [(QName,a)]
getStartValues analysis prog =
  if isSimpleAnalysis analysis
  then return []
  else do
    let startvals = case analysis of 
          DependencyFuncAnalysis _ _ _ -> 
            map (\func->(funcName func,startValue analysis)) 
                (progFuncs prog)
          CombinedDependencyFuncAnalysis _ _ _ _ _ -> 
            map (\func->(funcName func,startValue analysis))
                (progFuncs prog)
          DependencyTypeAnalysis _ _ _ -> 
            map (\typeDecl->(typeName typeDecl,startValue analysis))
                (progTypes prog)
          CombinedDependencyTypeAnalysis _ _ _ _ _ -> 
            map (\typeDecl->(typeName typeDecl,startValue analysis)) 
                (progTypes prog)
          _ -> error "Internal error in WorkerFunctions.getStartValues"
    return startvals

--- Compute a ProgInfo from a given list of infos for each function name w.r.t.
--- a given program.
funcInfos2ProgInfo :: Prog -> [(QName,a)] -> ProgInfo a
funcInfos2ProgInfo prog infos = lists2ProgInfo $
   map2 (\fdecl -> let fname = funcName fdecl
                    in (fname, fromJust (lookup fname infos)))
        (partition isVisibleFunc (progFuncs prog))

--- Compute a ProgInfo from a given list of infos for each type name w.r.t.
--- a given program.
typeInfos2ProgInfo :: Prog -> [(QName,a)] -> ProgInfo a
typeInfos2ProgInfo prog infos = lists2ProgInfo $
   map2 (\tdecl -> let tname = typeName tdecl
                    in (tname, fromJust (lookup tname infos)))
        (partition isVisibleType (progTypes prog))

map2 :: (a -> b) -> ([a], [a]) -> ([b], [b])
map2 f (xs,ys) = (map f xs, map f ys)

--- Update a given value list (second argument) w.r.t. new values given
--- in the first argument list.
updateList :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
updateList [] oldList = oldList
updateList ((key,newValue):newList) oldList =
  updateList newList (updateValue (key,newValue) oldList)

updateValue :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
updateValue _ [] = []
updateValue (key1,newValue) ((key2,value2):list) = 
  if key1==key2 then (key1,newValue):list
                else (key2,value2):(updateValue (key1,newValue) list)

-----------------------------------------------------------------------
execCombinedAnalysis :: Eq a => Analysis a -> Prog -> ProgInfo a -> [(QName,a)]
                     -> String -> String -> IO (ProgInfo a)
execCombinedAnalysis analysis prog importInfos startvals moduleName fpmethod =
 case analysis of
  CombinedSimpleFuncAnalysis _ ananame _ runWithBaseAna -> do
    anaFunc <- runWithBaseAna moduleName
    runAnalysis (SimpleFuncAnalysis ananame anaFunc)
                prog importInfos startvals fpmethod
  CombinedSimpleTypeAnalysis _ ananame _ runWithBaseAna -> do
    anaFunc <- runWithBaseAna moduleName
    runAnalysis (SimpleTypeAnalysis ananame anaFunc)
                prog importInfos startvals fpmethod
  CombinedDependencyFuncAnalysis _ ananame _ startval runWithBaseAna -> do
    anaFunc <- runWithBaseAna moduleName
    runAnalysis (DependencyFuncAnalysis ananame startval anaFunc)
                prog importInfos startvals fpmethod
  CombinedDependencyTypeAnalysis _ ananame _ startval runWithBaseAna -> do
    anaFunc <- runWithBaseAna moduleName
    runAnalysis (DependencyTypeAnalysis ananame startval anaFunc)
                prog importInfos startvals fpmethod
  _ -> error "Internal error in WorkerFunctions.execCombinedAnalysis"

-----------------------------------------------------------------------
--- Run an analysis but load default values (e.g., for external operations)
--- before and do not analyse the operations or types for these defaults.
runAnalysis :: Eq a => Analysis a -> Prog -> ProgInfo a -> [(QName,a)] -> String
            -> IO (ProgInfo a)
runAnalysis analysis prog importInfos startvals fpmethod = do
  deflts <- loadDefaultAnalysisValues (analysisName analysis) (progName prog)
  let defaultFuncs =
        updProgFuncs (filter (\fd -> funcName fd `elem`    map fst deflts)) prog
      definedFuncs =
        updProgFuncs (filter (\fd -> funcName fd `notElem` map fst deflts)) prog
      defaultTypes =
        updProgTypes (filter (\fd -> typeName fd `elem`    map fst deflts)) prog
      definedTypes =
        updProgTypes (filter (\fd -> typeName fd `notElem` map fst deflts)) prog
  let (progWithoutDefaults,defaultproginfo) = case analysis of
        SimpleFuncAnalysis _ _ ->
         (definedFuncs, funcInfos2ProgInfo defaultFuncs deflts)
        SimpleTypeAnalysis _ _ ->
         (definedTypes, typeInfos2ProgInfo defaultTypes deflts)
        SimpleConstructorAnalysis _ _ -> -- there are no external constructors
         if Prelude.null deflts then (prog,emptyProgInfo)
         else error "SimpleConstructorAnalysis with default values!"
        DependencyFuncAnalysis _ _ _ ->
         (definedFuncs, funcInfos2ProgInfo defaultFuncs deflts)
        DependencyTypeAnalysis _ _ _ ->
         (definedTypes, typeInfos2ProgInfo defaultTypes deflts)
        SimpleModuleAnalysis _ _ ->
         if Prelude.null deflts then (definedFuncs, emptyProgInfo)
                        else error defaultNotEmptyError
        DependencyModuleAnalysis _ _ ->
         if Prelude.null deflts then (definedFuncs, emptyProgInfo)
                        else error defaultNotEmptyError
        _ -> error "Internal error in WorkerFunctions.runAnalysis"
  let result = executeAnalysis analysis progWithoutDefaults
                               (combineProgInfo importInfos defaultproginfo)
                               startvals fpmethod
  return $ combineProgInfo defaultproginfo result
 where
  defaultNotEmptyError = "Default analysis information for analysis '" ++
                         analysisName analysis ++ "' and module '" ++
                         progName prog ++ "' not empty!"

--- Executes an anlysis on a given program w.r.t. an imported ProgInfo
--- and some start values (for dependency analysis).
--- The fixpoint iteration method to be applied is passed as the last argument.
executeAnalysis :: Eq a => Analysis a -> Prog -> ProgInfo a -> [(QName,a)]
                -> String
                -> ProgInfo a

-- The results of a module analysis for module `m` are encoded as
-- a `ProgInfo` with a single entry for the qualified name `m.m`.
executeAnalysis (SimpleModuleAnalysis _ anaFunc) prog _ _ _ =
 let pname = progName prog
 in lists2ProgInfo ([((pname,pname), anaFunc prog)], [])
executeAnalysis (DependencyModuleAnalysis _ anaFunc) prog impproginfos _ _ =
 let pname       = progName prog
     importinfos = map (\ (qn,a) -> (fst qn,a))
                       (publicListFromProgInfo impproginfos)
 in lists2ProgInfo ([((pname,pname), anaFunc prog importinfos)], [])

executeAnalysis (SimpleFuncAnalysis _ anaFunc) prog _ _ _ = 
  (lists2ProgInfo . map2 (\func -> (funcName func, anaFunc func))
                  . partition isVisibleFunc . progFuncs) prog

executeAnalysis (SimpleTypeAnalysis _ anaFunc) prog _ _ _ = 
  (lists2ProgInfo . map2 (\typ -> (typeName typ,anaFunc typ))
                  . partition isVisibleType . progTypes) prog

executeAnalysis (SimpleConstructorAnalysis _ anaFunc) prog _ _ _ = 
  (lists2ProgInfo
    . map2 (\ (cdecl,tdecl) -> (consName cdecl, anaFunc cdecl tdecl))
    . partition isVisibleCons
    . concatMap (\t -> map (\c->(c,t)) (consDeclsOfType t))
    . progTypes) prog
 where
  isVisibleCons (consDecl,_) = consVisibility consDecl == Public

executeAnalysis (DependencyFuncAnalysis _ _ anaFunc) prog
                importInfos startvals fpmethod = case fpmethod of
  "simple" ->
    let declsWithDeps = map2 addCalledFunctions
                             (partition isVisibleFunc (progFuncs prog))
        startinfo = funcInfos2ProgInfo prog startvals
     in simpleIteration anaFunc funcName declsWithDeps importInfos startinfo
  "wlist" ->
    let declsWithDeps = map addCalledFunctions (progFuncs prog)
     in funcInfos2ProgInfo prog $ fmToList $ 
          wlIteration anaFunc funcName declsWithDeps [] (empty (<))
                      importInfos (listToFM (<) startvals)
  "wlistscc" ->
    let declsWithDeps = map addCalledFunctions (progFuncs prog)
        -- compute strongly connected components w.r.t. func dependencies:
        sccDecls = scc ((:[]) . funcName . fst) snd declsWithDeps
     in funcInfos2ProgInfo prog $ fmToList $ 
          foldr (\scc sccstartvals ->
                   wlIteration anaFunc funcName scc [] (empty (<))
                               importInfos sccstartvals)
                (listToFM (<) startvals)
                (reverse sccDecls)
  _ -> error unknownFixpointMessage

executeAnalysis (DependencyTypeAnalysis _ _ anaType) prog
                importInfos startvals fpmethod = case fpmethod of
  "simple" ->
    let declsWithDeps = map2 addUsedTypes
                             (partition isVisibleType (progTypes prog))
        startinfo = typeInfos2ProgInfo prog startvals
     in simpleIteration anaType typeName declsWithDeps importInfos startinfo
  "wlist" ->
    let declsWithDeps = map addUsedTypes (progTypes prog)
     in typeInfos2ProgInfo prog $ fmToList $ 
          wlIteration anaType typeName declsWithDeps [] (empty (<))
                      importInfos (listToFM (<) startvals)
  "wlistscc" ->
    let declsWithDeps = map addUsedTypes (progTypes prog)
        -- compute strongly connected components w.r.t. type dependencies:
        sccDecls = scc ((:[]) . typeName . fst) snd declsWithDeps
     in typeInfos2ProgInfo prog $ fmToList $ 
          foldr (\scc sccstartvals ->
                   wlIteration anaType typeName scc [] (empty (<))
                               importInfos sccstartvals)
                (listToFM (<) startvals)
                (reverse sccDecls)
  _ -> error unknownFixpointMessage
-- These cases are handled elsewhere:
executeAnalysis (CombinedSimpleFuncAnalysis _ _ _ _) _ _ _ _ =
  error "Internal error in WorkerFunctions.executeAnalysis"
executeAnalysis (CombinedSimpleTypeAnalysis _ _ _ _) _ _ _ _ =
  error "Internal error in WorkerFunctions.executeAnalysis"
executeAnalysis (CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ _ _ =
  error "Internal error in WorkerFunctions.executeAnalysis"
executeAnalysis (CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ _ _ =
  error "Internal error in WorkerFunctions.executeAnalysis"

unknownFixpointMessage :: String
unknownFixpointMessage = "Unknown value for 'fixpoint' in configuration file!"

--- Add the directly called functions to each function declaration.
addCalledFunctions :: FuncDecl -> (FuncDecl,[QName])
addCalledFunctions func = (func, callsDirectly func)

--- Add the directly used type constructors to each type declaration.
addUsedTypes :: TypeDecl -> (TypeDecl,[QName])
addUsedTypes tdecl = (tdecl, dependsDirectlyOnTypes tdecl)

--- Gets all constructors of datatype declaration.
consDeclsOfType :: TypeDecl -> [ConsDecl]
consDeclsOfType (Type _ _ _ consDecls) = consDecls
consDeclsOfType (TypeSyn _ _ _ _) = []

-----------------------------------------------------------------------
--- Fixpoint iteration to compute analysis information. The arguments are:
--- * analysis operation
--- * operation to get name of a declaration
--- * list of public and private declarations together with their direct deps
--- * ProgInfo for imported entities
--- * current ProgInfo
--- Result: fixpoint ProgInfo
simpleIteration :: Eq a => (t -> [(QName,a)] -> a) -> (t -> QName)
                -> ([(t,[QName])],[(t,[QName])])
                -> ProgInfo a -> ProgInfo a -> ProgInfo a
simpleIteration analysis nameOf declsWithDeps importInfos currvals =
  let completeProgInfo = combineProgInfo currvals importInfos

      newvals =
        map2 (\ (decl,calls) ->
               (nameOf decl,
                analysis decl
                         (map (\qn -> (qn,fromJust -- information must known!
                                          (lookupProgInfo qn completeProgInfo)))
                               calls)))
             declsWithDeps

      newproginfo = lists2ProgInfo newvals

  in if equalProgInfo currvals newproginfo
     then currvals
     else simpleIteration analysis nameOf declsWithDeps importInfos newproginfo

wlIteration :: Eq a => (t -> [(QName,a)] -> a) -> (t -> QName)
            -> [(t,[QName])] -> [(t,[QName])] -> SetRBT QName
            -> ProgInfo a -> FM QName a -> FM QName a
--wlIteration analysis nameOf declsToDo declsDone changedEntities
--            importInfos currvals

wlIteration analysis nameOf [] alldecls changedEntities importInfos currvals =
  if Set.null changedEntities
  then currvals -- no todos, no changed values, so we are done:
  else -- all declarations processed, compute todos for next round:
       let (declsToDo,declsDone) =
              partition (\ (_,calls) -> any (`member` changedEntities) calls)
                        alldecls
        in wlIteration analysis nameOf declsToDo declsDone (empty (<))
                       importInfos currvals
-- process a single declaration:
wlIteration analysis nameOf (decldeps@(decl,calls):decls) declsDone
            changedEntities importInfos currvals =
  let decname = nameOf decl

      lookupVal qn = maybe (fromJust (lookupFM currvals qn)) id
                           (lookupProgInfo qn importInfos)
      oldval = lookupVal decname
      newval = analysis decl (map (\qn -> (qn, lookupVal qn)) calls)
   in if oldval==newval
      then wlIteration analysis nameOf decls (decldeps:declsDone)
                       changedEntities importInfos currvals
      else wlIteration analysis nameOf decls (decldeps:declsDone)
                       (insert decname changedEntities) importInfos
                       (updFM currvals decname (const newval))


---------------------------------------------------------------------
-- Auxiliaries

isVisibleFunc :: FuncDecl -> Bool
isVisibleFunc funcDecl = funcVisibility funcDecl == Public

isVisibleType :: TypeDecl -> Bool
isVisibleType typeDecl = typeVisibility typeDecl == Public

---------------------------------------------------------------------
