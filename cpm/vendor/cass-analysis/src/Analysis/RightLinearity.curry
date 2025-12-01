------------------------------------------------------------------------------
-- | Author : Michael Hanus
--   Version: November 2025
--
-- Right-linearity analysis:
-- check whether functions are defined by right-linear rules.
------------------------------------------------------------------------------

module Analysis.RightLinearity
  ( rlinAnalysis, hasRightLinearRules, linearExpr, showRightLinear)
 where

import Analysis.Types
import FlatCurry.Types
import Data.Maybe
import Data.List

------------------------------------------------------------------------------
-- | The right-linearity analysis is a global function dependency analysis.
--   It assigns to a function a flag which is True if this function
--   is right-linear, i.e., defined by right-linear rules and depend only on
--   functions defined by right-linear rules.

rlinAnalysis :: Analysis Bool
rlinAnalysis = dependencyFuncAnalysis "RightLinear" True rlFunc

-- An operation is right-linear if it is defined by right-linear rules
-- and depends only on right-linear operations.
rlFunc  :: FuncDecl -> [(QName,Bool)] -> Bool
rlFunc func calledFuncs =
  hasRightLinearRules func && all snd calledFuncs

-- | Show right-linearity information as a string.
showRightLinear :: AOutFormat -> Bool -> String
showRightLinear _     True  = "right-linear"
showRightLinear AText False = "not defined by right-linear rules"
showRightLinear ANote False = "" 

------------------------------------------------------------------------------
-- | The right-linearity analysis can also be applied to individual functions.
--   It returns True for a function if it is defined by right-linear rules.
hasRightLinearRules :: FuncDecl -> Bool
hasRightLinearRules (Func _ _ _ _ rule) = isRightLinearRule rule

isRightLinearRule :: Rule -> Bool
isRightLinearRule (Rule _ e)   = linearExpr e
isRightLinearRule (External _) = True

--------------------------------------------------------------------------

-- | Check an expression for linearity:
linearExpr :: Expr -> Bool
linearExpr e = maybe False (const True) (linearVariables e)

-- Return list of variables in an expression, if it is linear,
-- otherwise: Nothing
linearVariables :: Expr -> Maybe [Int]
linearVariables (Var i) = Just [i]
linearVariables (Lit _) = Just []
linearVariables (Comb _ f es)
 | f==("Prelude","?") && length es == 2  -- treat "?" as Or:
  = linearVariables (Or (head es) (head (tail es)))
 | otherwise
  = mapM linearVariables es >>= \esvars ->
    let vars = concat esvars
     in if nub vars == vars
        then Just vars
        else Nothing
linearVariables (Free vs e) =
  linearVariables e >>= \evars -> Just (evars \\ map fst vs)
linearVariables (Let bs e) =
  mapM linearVariables (expsOfLetBind bs) >>= \bsvars ->
  linearVariables e >>= \evars ->
  let vars = concat (evars : bsvars)
   in if nub vars == vars
      then Just (vars \\ (varsOfLetBind bs))
      else Nothing
linearVariables (Or e1 e2) =
  linearVariables e1 >>= \e1vars ->
  linearVariables e2 >>= \e2vars ->
  Just (union e1vars e2vars)
linearVariables (Case _ e bs) =
  linearVariables e >>= \evars ->
  mapM linearVariables (map (\ (Branch _ be) -> be) bs) >>= \bsvars ->
  let vars = foldr union [] (map (\ (branch,bsv) -> bsv \\ patternVars branch)
                                 (zip bs bsvars)) ++ evars
   in if nub vars == vars
      then Just vars
      else Nothing
 where
  patternVars (Branch (Pattern _ vs) _) = vs
  patternVars (Branch (LPattern _)   _) = []
linearVariables (Typed e _) = linearVariables e

--------------------------------------------------------------------------
