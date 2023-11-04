-----------------------------------------------------------------------------
--- An analysis to approximate the result values of operations
--- in a Curry program.
--- This analysis is parametric over a domain approximating
--- data terms, as defined in the library `Analysis.TermDomain`,
--- like a domain of outermost (top-level) constructors or
--- depth-bounded terms.
---
--- @author Michael Hanus
--- @version October 2023
-----------------------------------------------------------------------------

module Analysis.Values
  ( showValue
  , resultValueAnalysisTop, resultValueAnalysis2, resultValueAnalysis5 )
 where

import Data.List
import Test.Prop

import FlatCurry.Types

import Analysis.TermDomain
import Analysis.Types
import Analysis.ProgInfo

------------------------------------------------------------------------------
--- An abstract environments used in the analysis of a function associates
--- to each variable (index) an abstract type.
type AEnv atype = [(Int,atype)]

--- Extend an abstract environment with variables of any type:
extendEnv :: TermDomain a => AEnv a -> [Int] -> AEnv a
extendEnv env vars = zip vars (repeat anyType) ++ env

------------------------------------------------------------------------------

-- Shows an abstract value.
showValue :: TermDomain a => AOutFormat -> a -> String
showValue _ at = showType at

--- Result value analysis for the top-constructor domain.
resultValueAnalysisTop :: Analysis AType
resultValueAnalysisTop =
  dependencyFuncAnalysis "Values" emptyType analyseResultValues

--- Result value analysis for the depth-2 term domain.
resultValueAnalysis2 :: Analysis DType2
resultValueAnalysis2 =
  dependencyFuncAnalysis "Values2" emptyType analyseResultValues

--- Result value analysis for the depth-5 term domain.
resultValueAnalysis5 :: Analysis DType5
resultValueAnalysis5 =
  dependencyFuncAnalysis "Values5" emptyType analyseResultValues


analyseResultValues :: TermDomain a => FuncDecl -> [(QName,a)] -> a
analyseResultValues (Func (m,f) _ _ _ rule) calledfuncs
 | m == prelude = maybe (anaresult rule) id (lookup f preludeFuncs)
 | otherwise    = anaresult rule
 where
  -- add special results for prelude functions here:
  preludeFuncs = [ ("failed", emptyType)
                 , ("error", emptyType)
                 , ("prim_error", emptyType)
                 ]

  anaresult (External _)    = anyType
  anaresult (Rule args rhs) = anaExpr (extendEnv [] args) rhs

  anaExpr args exp = case exp of
    Var v         -> maybe anyType id (lookup v args)
    Lit l         -> aLit l
    Comb ct qf es -> if ct == FuncCall
                       then if qf == (prelude,"?") && length es == 2
                              then anaExpr args (Or (es!!0) (es!!1))
                              else maybe anyType id (lookup qf calledfuncs)
                       else aCons qf (map (anaExpr args) es)
    Let bs e      -> anaExpr (map (\ (v,ve) -> (v, anaExpr args ve)) bs ++ args)
                             e
    Free vs e     -> anaExpr (extendEnv args vs) e
    Or e1 e2      -> lubType (anaExpr args e1) (anaExpr args e2)
    Case _ _ bs   -> foldr lubType emptyType
                           (map (\ (Branch _ e) -> anaExpr args e) bs)
    Typed e _     -> anaExpr args e

-- Name of the standard prelude:
prelude :: String
prelude = "Prelude"

------------------------------------------------------------------------------
