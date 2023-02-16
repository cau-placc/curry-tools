------------------------------------------------------------------------------
--- Determinism analysis:
--- checks whether functions are deterministic or nondeterministic, i.e.,
--- whether its evaluation on ground argument terms might cause
--- different computation paths.
---
--- @author Michael Hanus
--- @version June 2022
------------------------------------------------------------------------------

module Analysis.Deterministic
  ( overlapAnalysis, showOverlap, showDet
  , functionalAnalysis, showFunctional, isNondetDefined
  , Deterministic(..),nondetAnalysis
  , showNonDetDeps, nondetDepAnalysis, nondetDepAllAnalysis
  ) where

import Analysis.Types
import FlatCurry.Types
import FlatCurry.Goodies
import Data.List
import Data.Char         (isDigit)

------------------------------------------------------------------------------
-- The overlapping analysis can be applied to individual functions.
-- It assigns to a FlatCurry function definition a flag which is True
-- if this function is defined with overlapping left-hand sides.

overlapAnalysis :: Analysis Bool
overlapAnalysis = simpleFuncAnalysis "Overlapping" isOverlappingFunction

isOverlappingFunction :: FuncDecl -> Bool
isOverlappingFunction (Func _ _ _ _ (Rule _ e))   = orInExpr e
isOverlappingFunction (Func f _ _ _ (External _)) = f == ("Prelude","?")

-- Check an expression for occurrences of OR:
orInExpr :: Expr -> Bool
orInExpr (Var _)       = False
orInExpr (Lit _)       = False
orInExpr (Comb _ f es) = f == (pre "?") || any orInExpr es
orInExpr (Free _ e)    = orInExpr e
orInExpr (Let bs e)    = any orInExpr (map snd bs) || orInExpr e
orInExpr (Or _ _)      = True
orInExpr (Case _ e bs) = orInExpr e || any orInBranch bs
 where orInBranch (Branch _ be) = orInExpr be
orInExpr (Typed e _) = orInExpr e

-- Show overlapping information as a string.
showOverlap :: AOutFormat -> Bool -> String
showOverlap _     True  = "overlapping"
showOverlap AText False = "non-overlapping"
showOverlap ANote False = ""

------------------------------------------------------------------------------
-- The functional analysis is a global function dependency analysis.
-- It assigns to a FlatCurry function definition a flag which is True
-- if this function is purely functional defined, i.e., its definition
-- does not depend on operation containing overlapping rules or free variables.

functionalAnalysis :: Analysis Bool
functionalAnalysis = dependencyFuncAnalysis "Functional" True isFuncDefined

-- Show functionally defined information as a string.
showFunctional :: AOutFormat -> Bool -> String
showFunctional _     True  = "functional"
showFunctional AText False = "defined with logic features"
showFunctional ANote False = ""

-- An operation is functionally defined if its definition is not
-- non-deterministic (no overlapping rules, no extra variables) and
-- it depends only on functionally defined operations.
isFuncDefined :: FuncDecl -> [(QName,Bool)] -> Bool
isFuncDefined func calledFuncs =
  not (isNondetDefined func) && all snd calledFuncs

-- Is a function f defined to be potentially non-deterministic, i.e.,
-- is the rule non-deterministic or does it contain extra variables?
isNondetDefined :: FuncDecl -> Bool
isNondetDefined (Func f _ _ _ rule) =
  f `notElem` (map pre ["failed","$!!","$##","normalForm","groundNormalForm"])
  -- these operations are internally defined in PAKCS with extra variables
  && isNondetRule rule
 where
  isNondetRule (Rule _ e) = orInExpr e || extraVarInExpr e
  isNondetRule (External _) = f==("Prelude","?")


-- check an expression for occurrences of extra variables:
extraVarInExpr :: Expr -> Bool
extraVarInExpr (Var _) = False
extraVarInExpr (Lit _) = False
extraVarInExpr (Comb _ _ es) = or (map extraVarInExpr es)
extraVarInExpr (Free vars e) = (not (null vars)) || extraVarInExpr e
extraVarInExpr (Let bs e) = any extraVarInExpr (map snd bs) || extraVarInExpr e
extraVarInExpr (Or e1 e2) = extraVarInExpr e1 || extraVarInExpr e2
extraVarInExpr (Case _  e bs) = extraVarInExpr e || any extraVarInBranch bs
                where extraVarInBranch (Branch _ be) = extraVarInExpr be
extraVarInExpr (Typed e _) = extraVarInExpr e

------------------------------------------------------------------------------
-- The determinism analysis is a global function dependency analysis.
-- It assigns to a function a flag which indicates whether is function
-- might be non-deterministic, i.e., might reduce in different ways
-- for given ground arguments.
-- If the non-determinism is encapsulated (set functions, getAllValues),
-- it is classified as deterministic.

--- Data type to represent determinism information.
data Deterministic = NDet | Det
  deriving (Eq, Ord, Show, Read)

-- Show determinism information as a string.
showDet :: AOutFormat -> Deterministic -> String
showDet _     NDet = "non-deterministic"
showDet AText Det  = "deterministic"
showDet ANote Det  = ""

nondetAnalysis :: Analysis Deterministic
nondetAnalysis = dependencyFuncAnalysis "Deterministic" Det nondetFunc

-- An operation is non-deterministic if its definition is potentially
-- non-deterministic or it calls a non-deterministic operation
-- where the non-deterministic call is not encapsulated.
nondetFunc :: FuncDecl -> [(QName,Deterministic)] -> Deterministic
nondetFunc func@(Func _ _ _ _ rule) calledFuncs =
  if isNondetDefined func || callsNDOpInRule rule
   then NDet
   else Det
 where
  callsNDOpInRule (Rule _ e) = callsNDOp e
  callsNDOpInRule (External _) = False

  callsNDOp (Var _)    = False
  callsNDOp (Lit _)    = False
  callsNDOp (Free _ e) = callsNDOp e
  callsNDOp (Let bs e) = any callsNDOp (map snd bs) || callsNDOp e
  callsNDOp (Or _ _)   = True
  callsNDOp (Case _ e bs) =
    callsNDOp e || any (\ (Branch _ be) -> callsNDOp be) bs
  callsNDOp (Typed e _) = callsNDOp e
  callsNDOp (Comb _ qf@(mn,fn) es)
    | mn == "SetFunctions" && take 3 fn == "set" && all isDigit (drop 3 fn)
    = -- non-determinism of function (first argument) is encapsulated so that
      -- its called ND functions are not relevant:
      if null es then False -- this case should not occur
                 else any callsNDOp (tail es)
    | isStrongEncapsOp qf
    = -- non-determinism of argument is encapsulated so that
      -- its called ND functions are not relevant:
      False
    | otherwise
    = maybe False (==NDet) (lookup qf calledFuncs) || any callsNDOp es

-- Does the operation ensures the strong encapsulation of its argument?
isStrongEncapsOp :: QName -> Bool
isStrongEncapsOp (mn,_) =
  mn `elem` ["Control.AllSolutions", "Control.AllValues"]

------------------------------------------------------------------------------
--- Data type to represent information about non-deterministic dependencies.
--- Basically, it is the set (represented as a sorted list) of
--- all function names that are defined by overlapping rules or rules
--- containing free variables which might be called.
--- In addition, the second component is (possibly) the list of
--- functions from which this non-deterministic function is called.
--- The length of this list is limited by 'maxDepsLength' in the
--- `NonDetAllDeps` analysis or to 1 (i.e., only the direct caller is
--- stored) in the `NonDetDeps` analysis.
type NonDetDeps = [(QName,[QName])]

--- The maximal length of a call chain associated with a non-deterministic
--- operation dependency. We limit the length in order to avoid large
--- analysis times for the `NonDetAllDeps` analysis.
maxDepsLength :: Int
maxDepsLength = 10

-- Show determinism dependency information as a string.
showNonDetDeps :: AOutFormat -> NonDetDeps -> String
showNonDetDeps AText []     = "deterministic"
showNonDetDeps ANote []     = ""
showNonDetDeps ANote xs@(_:_) = intercalate " " (nub (map (snd . fst) xs))
showNonDetDeps AText xs@(_:_) =
  "depends on non-det. operations: " ++
  intercalate ", " (map showNDOpInfo xs)
 where
  showNDOpInfo (ndop,cfs) = showQName ndop ++
    (if null cfs
      then ""
      else " (called from " ++ intercalate " -> " (map showQName cfs) ++ ")")

  showQName (mn,fn) = mn++"."++fn

--- Non-deterministic dependency analysis.
--- The analysis computes for each operation the set of operations
--- with a non-deterministic definition which might be called by this
--- operation. Non-deterministic operations that are called by other
--- non-deterministic operations are ignored so that only the first
--- (w.r.t. the call sequence) non-deterministic operations are returned.
nondetDepAnalysis :: Analysis NonDetDeps
nondetDepAnalysis =
  dependencyFuncAnalysis "NonDetDeps" [] (nondetDeps False)

--- Non-deterministic dependency analysis.
--- The analysis computes for each operation the set of *all* operations
--- with a non-deterministic definition which might be called by this
--- operation.
nondetDepAllAnalysis :: Analysis NonDetDeps
nondetDepAllAnalysis =
  dependencyFuncAnalysis "NonDetAllDeps" [] (nondetDeps True)

-- An operation is non-deterministic if its definition is potentially
-- non-deterministic (i.e., the dependency is the operation itself)
-- or it depends on some called non-deterministic function.
nondetDeps :: Bool -> FuncDecl -> [(QName,NonDetDeps)] -> NonDetDeps
nondetDeps alldeps func@(Func f _ _ _ rule) calledFuncs =
  let calledndfuncs = sort (nub (map addCaller (calledNDFuncsInRule rule)))
      addCaller (ndf,cfs)
        | null cfs                      = (ndf,[f])
        | alldeps && f `notElem` cfs
          && length cfs < maxDepsLength = (ndf,f:cfs)
        | otherwise                     = (ndf,cfs)
   in if isNondetDefined func
       then (f,[]) : (if alldeps then calledndfuncs else [])
       else calledndfuncs
 where
  calledNDFuncsInRule (Rule _ e) = calledNDFuncs e
  calledNDFuncsInRule (External _) = []

  calledNDFuncs (Var _) = []
  calledNDFuncs (Lit _) = []
  calledNDFuncs (Free _ e) = calledNDFuncs e
  calledNDFuncs (Let bs e) =
    concatMap calledNDFuncs (map snd bs) ++ calledNDFuncs e
  calledNDFuncs (Or e1 e2) = calledNDFuncs e1 ++ calledNDFuncs e2
  calledNDFuncs (Case _ e bs) =
    calledNDFuncs e ++ concatMap (\ (Branch _ be) -> calledNDFuncs be) bs
  calledNDFuncs (Typed e _) = calledNDFuncs e
  calledNDFuncs (Comb _ qf@(mn,fn) es)
    | mn == "SetFunctions" && take 3 fn == "set" && all isDigit (drop 3 fn)
    = -- non-determinism of function (first argument) is encapsulated so that
      -- its called ND functions are not relevant:
      if null es then [] -- this case should not occur
                 else concatMap calledNDFuncs (tail es)
    | isStrongEncapsOp qf
    = -- non-determinism of argument is encapsulated so that
      -- its called ND functions are not relevant:
      []
    | otherwise
    = maybe [] id (lookup qf calledFuncs) ++ concatMap calledNDFuncs es

------------------------------------------------------------------------------

pre :: String -> QName
pre n = ("Prelude",n)

------------------------------------------------------------------------------
