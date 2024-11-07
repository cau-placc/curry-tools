-----------------------------------------------------------------------------
--- Pattern completeness and totally definedness analysis for Curry programs
---
--- This analysis checks for each function in a Curry program  whether
--- this function is completely defined, i.e., reducible on all ground
--- constructor terms
---
--- @author Johannes Koj, Michael Hanus
--- @version November 2024
-----------------------------------------------------------------------------

{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Analysis.TotallyDefined
  ( siblingCons, showSibling
  , siblingConsAndDecl, showSiblingAndDecl
  , Completeness(..), showComplete, showTotally, showTotalFunc
  , patCompAnalysis, totalAnalysis, totalFuncAnalysis
  ) where

import FlatCurry.Types
import FlatCurry.Goodies
import Data.List              ( delete )
import RW.Base
import System.IO

import Analysis.ProgInfo
import Analysis.Types
import Analysis.Deterministic ( isNondetDefined )

-----------------------------------------------------------------------
--- An analysis to compute the sibling constructors (belonging to the
--- same data type) for a data constructor.

--- Shows the result of the sibling constructors analysis, i.e.,
--- shows a list of constructor names together with their arities.
showSibling :: AOutFormat -> [(QName,Int)] -> String
showSibling _ = show

siblingCons :: Analysis [(QName,Int)]
siblingCons = simpleConstructorAnalysis "SiblingCons" consNamesArOfType
 where
  -- get all constructor names and arities of a datatype declaration
  consNamesArOfType cdecl (Type _ _ _ consDecls) =
    map (\cd -> (consName cd, consArity cd))
        (filter (\cd -> consName cd /= consName cdecl) consDecls)
  consNamesArOfType _ (TypeSyn _ _ _ _) = []
  consNamesArOfType _ (TypeNew _ _ _ _) = []

-----------------------------------------------------------------------
--- An analysis to compute the sibling constructors (belonging to the
--- same data type) and type declaration for a data constructor.

--- Shows the result of the sibling constructors analysis, i.e.,
--- shows a tuple of a type declaration and a list of constructor
--- names together with their arities.
showSiblingAndDecl :: AOutFormat -> (TypeDecl, [(QName,Int)]) -> String
showSiblingAndDecl _ = show

siblingConsAndDecl :: Analysis (TypeDecl, [(QName,Int)])
siblingConsAndDecl =
  simpleConstructorAnalysis "SiblingConsAndDecl" consNamesArOfType
 where
  -- get all constructor names and arities of a datatype declaration
  consNamesArOfType cdecl t = (t, cs)
    where cs = case t of
            Type _ _ _ consDecls ->
              map (\cd -> (consName cd, consArity cd))
                (filter (\cd -> consName cd /= consName cdecl) consDecls)
            TypeSyn _ _ _ _ -> []
            TypeNew _ _ _ _ -> []

------------------------------------------------------------------------------
-- The completeness analysis assigns to an operation a flag indicating
-- whether this operation is completely defined on its input types,
-- i.e., reducible for all ground data terms.

-- The possible outcomes of the completeness analysis:
data Completeness =
     Complete       -- completely defined
   | InComplete     -- incompletely defined
   | InCompleteOr   -- incompletely defined in each branch of an "Or"
 deriving (Eq, Show, Read)

--- Pattern completeness analysis
patCompAnalysis :: Analysis Completeness
patCompAnalysis =
  combinedSimpleFuncAnalysis "PatComplete" siblingCons analysePatComplete

-- Shows the result of the completeness analysis.
showComplete :: AOutFormat -> Completeness -> String
showComplete AText Complete     = "complete"
showComplete ANote Complete     = ""
showComplete _     InComplete   = "incomplete"
showComplete _     InCompleteOr = "incomplete in each disjunction"


analysePatComplete :: ProgInfo [(QName,Int)] -> FuncDecl -> Completeness
analysePatComplete consinfo fdecl = anaFun fdecl
 where
  anaFun (Func _ _ _ _ (Rule _ e)) = isComplete consinfo e
  anaFun (Func _ _ _ _ (External _)) = Complete

isComplete :: ProgInfo [(QName,Int)] -> Expr -> Completeness
isComplete _ (Var _)      = Complete
isComplete _ (Lit _)      = Complete
isComplete consinfo (Comb _ f es)
  -- branches with Prelude.failed as right-hand side are incomplete:
  | f == ("Prelude","failed")                   = InComplete
  | f == ("Prelude","commit") && length es == 1 = isComplete consinfo (head es)
  | otherwise                                   = Complete
isComplete _ (Free _ _) = Complete
isComplete _ (Let _ _) = Complete
isComplete consinfo (Or e1 e2) =
   combineOrResults (isComplete consinfo e1) (isComplete consinfo e2)
-- if there is no branch, it is incomplete:
isComplete _ (Case _ _ []) = InComplete
-- for literal branches we assume that not all alternatives are provided:
isComplete _ (Case _ _ (Branch (LPattern _)   _ : _)) = InComplete
isComplete consinfo (Case _ _ (Branch (Pattern cons _) bexp : ces)) =
    combineAndResults
      (checkAllCons (maybe [] (map fst) (lookupProgInfo cons consinfo)) ces)
      (isComplete consinfo bexp)
  where
   -- check for occurrences of all constructors in each case branch:
   checkAllCons []    _  = Complete
   checkAllCons (_:_) [] = InComplete
   checkAllCons (_:_)
                (Branch (LPattern _)   _ : _) = InComplete -- should not occur
   checkAllCons (c:cs) (Branch (Pattern i _) e : ps) =
     combineAndResults (checkAllCons (delete i (c:cs)) ps)
                       (isComplete consinfo e)
isComplete consinfo (Typed e _) = isComplete consinfo e

-- Combines the completeness results in different Or branches.
combineOrResults :: Completeness -> Completeness -> Completeness
combineOrResults Complete     _            = Complete
combineOrResults InComplete   Complete     = Complete
combineOrResults InComplete   InComplete   = InCompleteOr
combineOrResults InComplete   InCompleteOr = InCompleteOr
combineOrResults InCompleteOr Complete     = Complete
combineOrResults InCompleteOr InComplete   = InCompleteOr
combineOrResults InCompleteOr InCompleteOr = InCompleteOr

-- Combines the completeness results in different case branches.
combineAndResults :: Completeness -> Completeness -> Completeness
combineAndResults InComplete   _            = InComplete
combineAndResults Complete     Complete     = Complete
combineAndResults Complete     InComplete   = InComplete
combineAndResults Complete     InCompleteOr = InCompleteOr
combineAndResults InCompleteOr Complete     = InCompleteOr
combineAndResults InCompleteOr InComplete   = InComplete
combineAndResults InCompleteOr InCompleteOr = InCompleteOr

------------------------------------------------------------------------------
--- An operation is totally defined if it is pattern complete and depends
--- only on totally defined functions.
totalAnalysis :: Analysis Bool
totalAnalysis =
  combinedDependencyFuncAnalysis "Total" patCompAnalysis True analyseTotally

analyseTotally :: ProgInfo Completeness -> FuncDecl -> [(QName,Bool)] -> Bool
analyseTotally pcinfo fdecl calledfuncs =
  (maybe False (== Complete) (lookupProgInfo (funcName fdecl) pcinfo))
  && all snd calledfuncs

-- Shows the result of the totally-defined analysis.
showTotally :: AOutFormat -> Bool -> String
showTotally AText True  = "totally defined"
showTotally ANote True  = ""
showTotally _     False = "partially defined"

------------------------------------------------------------------------------
-- The completeness analysis assigns to an operation a flag indicating
-- whether this operation is completely defined on its input types,
-- i.e., reducible for all ground data terms.

-- Shows the result of the totally-defined function analysis.
showTotalFunc :: AOutFormat -> Bool -> String
showTotalFunc AText True  = "totally and functionally defined"
showTotalFunc ANote True  = ""
showTotalFunc AText False = "partially or non-deterministically defined"
showTotalFunc ANote False = "partial/non-deterministic"

--- An operation is totally and functionally defined if it is totally defined
--- and functionally defined (see `Analysis.Deterministic`).
--- Thus, it is defined without any pattern failures in the sense
--- of purely functional programming.
totalFuncAnalysis :: Analysis Bool
totalFuncAnalysis =
  combinedDependencyFuncAnalysis "TotalFunc" totalAnalysis True analyseTotalFunc

analyseTotalFunc :: ProgInfo Bool -> FuncDecl -> [(QName,Bool)] -> Bool
analyseTotalFunc pcinfo fdecl calledfuncs =
  (maybe False id (lookupProgInfo (funcName fdecl) pcinfo))
  && not (isNondetDefined fdecl)
  && all snd calledfuncs

------------------------------------------------------------------------------
-- ReadWrite instances:

instance ReadWrite Completeness where
  readRW _ ('0' : r0) = (Complete,r0)
  readRW _ ('1' : r0) = (InComplete,r0)
  readRW _ ('2' : r0) = (InCompleteOr,r0)

  showRW _ strs0 Complete = (strs0,showChar '0')
  showRW _ strs0 InComplete = (strs0,showChar '1')
  showRW _ strs0 InCompleteOr = (strs0,showChar '2')

  writeRW _ h Complete strs = hPutChar h '0' >> return strs
  writeRW _ h InComplete strs = hPutChar h '1' >> return strs
  writeRW _ h InCompleteOr strs = hPutChar h '2' >> return strs

  typeOf _ = monoRWType "Completeness"

------------------------------------------------------------------------------
