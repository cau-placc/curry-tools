-------------------------------------------------------------------------
--- This module contains the datatypes, constructors, and other
--- operations to create and process analyses used in the
--- generic analysis system.
---
--- Each analysis has a name which is used to identify the analysis
--- stored in files, when passing analysis information between workers etc.
---
--- **Important:** Use the constructor operations to define new analyses
---                (instead of the data constructors).
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version June 2018
-------------------------------------------------------------------------

module Analysis.Types
  ( Analysis(..)
  , simpleFuncAnalysis, simpleTypeAnalysis, simpleConstructorAnalysis
  , dependencyFuncAnalysis, dependencyTypeAnalysis
  , combinedSimpleFuncAnalysis, combined2SimpleFuncAnalysis
  , combinedSimpleTypeAnalysis
  , combinedDependencyFuncAnalysis, combinedDependencyTypeAnalysis
  , simpleModuleAnalysis, dependencyModuleAnalysis
  , isSimpleAnalysis, isCombinedAnalysis, isFunctionAnalysis
  , analysisName, baseAnalysisNames, startValue
  , AOutFormat(..)
  ) where

import FlatCurry.Types   ( Prog, ConsDecl, FuncDecl, TypeDecl, QName )
import FlatCurry.Goodies ( progImports )

import Analysis.ProgInfo ( ProgInfo, combineProgInfo, lookupProgInfo )
import Analysis.Files    ( getImports, loadCompleteAnalysis, getInterfaceInfos )

--- Datatype representing a program analysis to be used in the
--- generic analysis system. The datatype is abstract so that
--- one has to use one of the constructor operations to create
--- an analysis.
data Analysis a = 
   SimpleFuncAnalysis String (FuncDecl -> a)
 | SimpleTypeAnalysis String (TypeDecl -> a)
 | SimpleConstructorAnalysis String (ConsDecl -> TypeDecl -> a)
 | DependencyFuncAnalysis String a (FuncDecl -> [(QName,a)] -> a)
 | DependencyTypeAnalysis String a (TypeDecl -> [(QName,a)] -> a)
 | CombinedSimpleFuncAnalysis [String] String Bool
                              (String -> IO (FuncDecl -> a))
 | CombinedSimpleTypeAnalysis [String] String Bool
                              (String -> IO (TypeDecl -> a))
 | CombinedDependencyFuncAnalysis [String] String Bool a
                                  (String -> IO (FuncDecl -> [(QName,a)] -> a))
 | CombinedDependencyTypeAnalysis [String] String Bool a
                                  (String -> IO (TypeDecl -> [(QName,a)] -> a))
 | SimpleModuleAnalysis     String (Prog -> a)
 | DependencyModuleAnalysis String (Prog -> [(String,a)] -> a)


--- A simple analysis for functions takes an operation that computes
--- some information from a given function declaration.
simpleFuncAnalysis :: String -> (FuncDecl -> a) -> Analysis a
simpleFuncAnalysis anaName anaFunc =
  SimpleFuncAnalysis anaName anaFunc 

--- A simple analysis for types takes an operation that computes
--- some information from a given type declaration.
simpleTypeAnalysis :: String -> (TypeDecl -> a) -> Analysis a
simpleTypeAnalysis anaName anaFunc =
  SimpleTypeAnalysis anaName anaFunc 

--- A simple analysis for data constructors takes an operation that computes
--- some information for a constructor declaration and its type declaration
--- to which it belongs.
simpleConstructorAnalysis :: String -> (ConsDecl -> TypeDecl -> a) -> Analysis a
simpleConstructorAnalysis anaName anaFunc =
  SimpleConstructorAnalysis anaName anaFunc 

--- Construct a function analysis with dependencies.
--- The analysis has a name, a start value (representing "no initial
--- information") and an operation to process a function declaration
--- with analysis information
--- for the operations directly called in this function declaration.
--- The analysis will be performed by a fixpoint iteration
--- starting with the given start value.
dependencyFuncAnalysis :: String -> a -> (FuncDecl -> [(QName,a)] -> a)
                       -> Analysis a
dependencyFuncAnalysis anaName startval anaFunc =
  DependencyFuncAnalysis anaName startval anaFunc

--- Construct a type analysis with dependencies.
--- The analysis has a name, a start value (representing "no initial
--- information") and an operation to process a type declaration
--- with analysis information
--- for the type constructors occurring in the type declaration.
--- The analysis will be performed by a fixpoint iteration
--- starting with the given start value.
dependencyTypeAnalysis :: String -> a -> (TypeDecl -> [(QName,a)] -> a)
                       -> Analysis a
dependencyTypeAnalysis anaName startval anaType =
  DependencyTypeAnalysis anaName startval anaType

--- A simple combined analysis for functions.
--- The analysis is based on an operation that computes
--- some information from a given function declaration
--- and information provided by some base analysis.
--- The base analysis is provided as the second argument.
combinedSimpleFuncAnalysis :: String -> Analysis b
                           -> (ProgInfo  b -> FuncDecl -> a) -> Analysis a
combinedSimpleFuncAnalysis ananame baseAnalysis anaFunc =
  CombinedSimpleFuncAnalysis [analysisName baseAnalysis] ananame True
                             (runWithBaseAnalysis baseAnalysis anaFunc)

--- A simple combined analysis for functions.
--- The analysis is based on an operation that computes
--- some information from a given function declaration
--- and information provided by two base analyses.
--- The base analyses are provided as the second and third argument.
combined2SimpleFuncAnalysis :: String -> Analysis b -> Analysis c
                  -> (ProgInfo b -> ProgInfo c -> FuncDecl -> a) -> Analysis a
combined2SimpleFuncAnalysis ananame baseAnalysisA baseAnalysisB anaFunc =
  CombinedSimpleFuncAnalysis
    [analysisName baseAnalysisA, analysisName baseAnalysisB]
    ananame
    True
    (runWith2BaseAnalyses baseAnalysisA baseAnalysisB anaFunc)

--- A simple combined analysis for types.
--- The analysis is based on an operation that computes
--- some information from a given type declaration
--- and information provided by some base analysis.
--- The base analysis is provided as the second argument.
combinedSimpleTypeAnalysis :: String -> Analysis b
                           -> (ProgInfo  b -> TypeDecl -> a) -> Analysis a
combinedSimpleTypeAnalysis ananame baseAnalysis anaFunc =
  CombinedSimpleTypeAnalysis [analysisName baseAnalysis] ananame True
                             (runWithBaseAnalysis baseAnalysis anaFunc)

--- A combined analysis for functions with dependencies.
--- The analysis is based on an operation that computes
--- from information provided by some base analysis
--- for each function declaration and information about its
--- directly called operation some information for the declared function.
--- The analysis will be performed by a fixpoint iteration
--- starting with the given start value (fourth argument).
--- The base analysis is provided as the second argument.
combinedDependencyFuncAnalysis :: String -> Analysis b -> a
             -> (ProgInfo b -> FuncDecl -> [(QName,a)] -> a) -> Analysis a
combinedDependencyFuncAnalysis ananame baseAnalysis startval anaFunc =
  CombinedDependencyFuncAnalysis
    [analysisName baseAnalysis] ananame True startval
    (runWithBaseAnalysis baseAnalysis anaFunc)

--- A combined analysis for types with dependencies.
--- The analysis is based on an operation that computes
--- from information provided by some base analysis
--- for each type declaration and information about its
--- directly used types some information for the declared type.
--- The analysis will be performed by a fixpoint iteration
--- starting with the given start value (fourth argument).
--- The base analysis is provided as the second argument.
combinedDependencyTypeAnalysis :: String -> Analysis b -> a
   -> (ProgInfo b -> TypeDecl -> [(QName,a)] -> a) -> Analysis a
combinedDependencyTypeAnalysis ananame baseAnalysis startval anaType =
  CombinedDependencyTypeAnalysis
    [analysisName baseAnalysis] ananame True startval
    (runWithBaseAnalysis baseAnalysis anaType)

--- Construct a simple analysis for entire modules.
--- The analysis has a name and takes an operation that computes
--- some information from a given module.
simpleModuleAnalysis :: String -> (Prog -> a) -> Analysis a
simpleModuleAnalysis anaName anaFunc =
  SimpleModuleAnalysis anaName anaFunc 

--- Construct a module analysis which uses analysis information on
--- imported modules.
--- The analysis has a name and an operation to analyze a module.
--- The analysis operation could use already computed information
--- of imported modules, represented as a list of module name/information pairs.
--- Note that a fixpoint iteration is not necessary
--- since module dependencies must be acyclic.
dependencyModuleAnalysis :: String -> (Prog -> [(String,a)] -> a) -> Analysis a
dependencyModuleAnalysis anaName anaFunc =
  DependencyModuleAnalysis anaName anaFunc


-------------------------------------------------------------------------

--- Is the analysis a simple analysis?
--- Otherwise, it is a dependency analysis which requires a fixpoint
--- computation to compute the results.
isSimpleAnalysis :: Analysis a -> Bool
isSimpleAnalysis analysis = case analysis of
  SimpleFuncAnalysis         _ _     -> True
  SimpleTypeAnalysis         _ _     -> True
  SimpleConstructorAnalysis  _ _     -> True
  CombinedSimpleFuncAnalysis _ _ _ _ -> True
  CombinedSimpleTypeAnalysis _ _ _ _ -> True
  _                                  -> False

--- Is the analysis a combined analysis?
isCombinedAnalysis :: Analysis a -> Bool
isCombinedAnalysis analysis = case analysis of
  CombinedSimpleFuncAnalysis     _ _ _ _   -> True
  CombinedSimpleTypeAnalysis     _ _ _ _   -> True
  CombinedDependencyFuncAnalysis _ _ _ _ _ -> True
  CombinedDependencyTypeAnalysis _ _ _ _ _ -> True
  _                                        -> False

--- Is the analysis a function analysis?
--- Otherwise, it is a type or constructor analysis.
isFunctionAnalysis :: Analysis a -> Bool
isFunctionAnalysis analysis = case analysis of
  SimpleFuncAnalysis             _ _       -> True
  DependencyFuncAnalysis         _ _ _     -> True
  CombinedSimpleFuncAnalysis     _ _ _ _   -> True
  CombinedDependencyFuncAnalysis _ _ _ _ _ -> True
  _                                        -> False

--- Name of the analysis to be used in server communication and
--- analysis files.
analysisName :: Analysis a -> String
analysisName (SimpleFuncAnalysis        name _  ) = name
analysisName (SimpleTypeAnalysis        name _  ) = name
analysisName (SimpleConstructorAnalysis name _  ) = name
analysisName (DependencyFuncAnalysis    name _ _) = name
analysisName (DependencyTypeAnalysis    name _ _) = name
analysisName (CombinedSimpleFuncAnalysis _ nameB _ _) = nameB
analysisName (CombinedSimpleTypeAnalysis _ nameB _ _) = nameB
analysisName (CombinedDependencyFuncAnalysis _ nameB _ _ _) = nameB
analysisName (CombinedDependencyTypeAnalysis _ nameB _ _ _) = nameB
analysisName (SimpleModuleAnalysis           name _) = name
analysisName (DependencyModuleAnalysis       name _) = name

--- Names of the base analyses of a combined analysis.
baseAnalysisNames :: Analysis a -> [String]
baseAnalysisNames ana = case ana of
  CombinedSimpleFuncAnalysis     bnames _ _ _   -> bnames
  CombinedSimpleTypeAnalysis     bnames _ _ _   -> bnames
  CombinedDependencyFuncAnalysis bnames _ _ _ _ -> bnames
  CombinedDependencyTypeAnalysis bnames _ _ _ _ -> bnames
  _                                             -> []

--- Start value of a dependency analysis.
startValue :: Analysis a -> a
startValue ana = case ana of
  DependencyFuncAnalysis _             startval _ -> startval
  DependencyTypeAnalysis _             startval _ -> startval 
  CombinedDependencyFuncAnalysis _ _ _ startval _ -> startval
  CombinedDependencyTypeAnalysis _ _ _ startval _ -> startval
  _ -> error "Internal error in Analysis.startValue"

-------------------------------------------------------------------------
--- The desired kind of output of an analysis result.
--- `AText` denotes a standard textual representation.
--- `ANote` denotes a short note that is empty in case of irrelevant
--- information. For instance, this is used in the CurryBrowser
--- to get a quick overview of the analysis results of all operations
--- in a module.
data AOutFormat = AText | ANote

-------------------------------------------------------------------------
--- Loads the results of the base analysis and put it as the first
--- argument of the main analysis operation which is returned.
runWithBaseAnalysis :: Analysis a -> (ProgInfo a -> (input -> b)) -> String
                    -> IO (input -> b)
runWithBaseAnalysis baseAnalysis analysisFunction moduleName = do
  importedModules <- getImports moduleName
  let baseananame = analysisName baseAnalysis
  impbaseinfos  <- getInterfaceInfos baseananame importedModules
  mainbaseinfos <- loadCompleteAnalysis baseananame moduleName
  let baseinfos = combineProgInfo impbaseinfos mainbaseinfos
  return (analysisFunction baseinfos)

--- Loads the results of the base analysis and put it as the first
--- argument of the main analysis operation which is returned.
runWith2BaseAnalyses :: Analysis a -> Analysis b
                     -> (ProgInfo a -> ProgInfo b -> (input -> c)) -> String
                     -> IO (input -> c)
runWith2BaseAnalyses baseanaA baseanaB analysisFunction moduleName = do
  importedModules <- getImports moduleName
  let baseananameA = analysisName baseanaA
      baseananameB = analysisName baseanaB
  impbaseinfosA  <- getInterfaceInfos baseananameA importedModules
  mainbaseinfosA <- loadCompleteAnalysis baseananameA moduleName
  impbaseinfosB  <- getInterfaceInfos baseananameB importedModules
  mainbaseinfosB <- loadCompleteAnalysis baseananameB moduleName
  let baseinfosA = combineProgInfo impbaseinfosA mainbaseinfosA
      baseinfosB = combineProgInfo impbaseinfosB mainbaseinfosB
  return (analysisFunction baseinfosA baseinfosB)

