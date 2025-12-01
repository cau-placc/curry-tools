------------------------------------------------------------------------------
-- | Author : Michael Hanus
--   Version: November 2025
--
-- Analysis for solution completeness:
-- check whether functions are solution complete, i.e., calls only
-- non-rigid functions
------------------------------------------------------------------------------

module Analysis.SolutionCompleteness
  ( solcompAnalysis, showSolComplete )
 where

import Analysis.Types
import FlatCurry.Types
import Data.List

------------------------------------------------------------------------------

-- | The completeness analysis is a global function dependency analysis.
--   It assigns to a function a flag which is True if this function
--   is operationally complete, i.e., does not call (explicitly or implicitly)
--   a rigid function.
solcompAnalysis :: Analysis Bool
solcompAnalysis = dependencyFuncAnalysis "SolComplete" True scFunc

-- An operation is solution complete if it is defined with flexible
-- rules and depends only on solution complete operations.
scFunc  :: FuncDecl -> [(QName,Bool)] -> Bool
scFunc func calledFuncs =
  isFlexDefined func && all snd calledFuncs

-- (isFlexDefined fundecl):
-- Is a function defined by a flexible rule?
isFlexDefined :: FuncDecl -> Bool
isFlexDefined (Func _ _ _ _ (Rule _ e)) = isFlexExpr e
isFlexDefined (Func f _ _ _ (External _)) =
   f `elem` map pre ["=:=","success","&","&>","return"]

-- Checks whether an expression is flexible, i.e., can only suspend
-- because of calls to other possibly rigid functions.

isFlexExpr :: Expr -> Bool
isFlexExpr (Var _)           = True
isFlexExpr (Lit _)           = True
isFlexExpr (Comb _ f args) =
       f/=(pre "apply") -- apply suspends if arg 1 is unbound
    && f/=(pre "commit")
    && all isFlexExpr args
isFlexExpr (Free _ e)        = isFlexExpr e
isFlexExpr (Let bs e)        = all isFlexExpr (expsOfLetBind bs) &&
                               isFlexExpr e
isFlexExpr (Or e1 e2)        = isFlexExpr e1 && isFlexExpr e2
isFlexExpr (Case ctype e bs) = ctype==Flex &&
                               all isFlexExpr (e : map (\(Branch _ be) ->be) bs)
isFlexExpr (Typed e _)       = isFlexExpr e

-- | Show solution completeness information as a string.
showSolComplete :: AOutFormat -> Bool -> String
showSolComplete _ True  = "solution complete"
showSolComplete _ False = "maybe suspend"


pre :: String -> QName
pre n = ("Prelude",n)

------------------------------------------------------------------------------
