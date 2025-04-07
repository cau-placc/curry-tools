------------------------------------------------------------------------------
--- Indeterminism analysis:
--- check whether functions are indeterministic, i.e., might deliver
--- different results for different runs of a program.
--- This could be the case if there are explicit or implicit calls
--- to indeterministic encapsulation operation
--- (e.g. `Control.Search.SetFunctions.select`) or operations from the module
--- `System.IO.Unsafe`.
---
--- @author Michael Hanus
--- @version February 2025
------------------------------------------------------------------------------

module Analysis.Indeterministic ( indetAnalysis, showIndet ) where

import Analysis.Types
import FlatCurry.Types

------------------------------------------------------------------------------
--- The indeterminism analysis is a global function dependency analysis.
--- It assigns to a function a flag which is True if this function
--- might be indeterministic (i.e., calls directly or indirectly
--- some indeterministic operation.

indetAnalysis :: Analysis Bool
indetAnalysis = dependencyFuncAnalysis "Indeterministic" False indetFunc

--- An operation is indeterministic if it calls directly or indirectly
--- some indeterministic operation.
indetFunc  :: FuncDecl -> [(QName,Bool)] -> Bool
indetFunc func calledFuncs =
  hasIndetRules func || any snd calledFuncs

-- Show right-linearity information as a string.
showIndet :: AOutFormat -> Bool -> String
showIndet AText True  = "impure (indeterministic) operation"
showIndet ANote True  = "indeterministic"
showIndet AText False = "referentially transparent operation"
showIndet ANote False = "" 

------------------------------------------------------------------------------

-- Check a function declaration for occurrences of indeterministic operations.
hasIndetRules :: FuncDecl -> Bool
hasIndetRules (Func _ _ _ _ (Rule _ e))   = indetInExpr e
hasIndetRules (Func _ _ _ _ (External _)) = False

-- Check an expression for occurrences of indeterministic operations.
indetInExpr :: Expr -> Bool
indetInExpr (Var _) = False
indetInExpr (Lit _) = False
indetInExpr (Comb _ f es) = isIndetFunction f || any indetInExpr es
indetInExpr (Free _ e) = indetInExpr e
indetInExpr (Let bs e) = any indetInExpr (map snd bs) || indetInExpr e
indetInExpr (Or e1 e2) = indetInExpr e1 || indetInExpr e2
indetInExpr (Case _  e bs) = indetInExpr e || any indetInBranch bs
                where indetInBranch (Branch _ be) = indetInExpr be
indetInExpr (Typed e _) = indetInExpr e

-- Is the given operation indeterministic?
isIndetFunction :: QName -> Bool
isIndetFunction (mn,fn) =
  (mn `elem` ["Control.Search.SetFunctions", "Control.SetFunctions"] &&
   fn `elem` ["select", "selectValue"]) ||
  mn `elem` ["System.IO.Unsafe", "Control.Search.Unsafe", "Control.AllValues",
             "Control.Findall", "Control.Search.SearchTree.Unsafe"] ||
  (mn == "Network.Ports" &&
   fn `elem` ["send", "timeoutOnStream", "choiceSPEP"])

-------------------------------------------------------------------------------
