------------------------------------------------------------------------------
--- Termination analysis:
--- checks whether an operation is terminating, i.e.,
--- whether all evaluations on ground argument terms are finite.
--- The method used here checks whether the arguments in all recursive
--- calls of an operation are smaller than the arguments passed to
--- the operation.
---
--- @author Michael Hanus
--- @version November 2024
------------------------------------------------------------------------------

{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}

module Analysis.Termination
  ( terminationAnalysis, showTermination
  , productivityAnalysis, showProductivity, Productivity(..)
  ) where

import Data.Char (isDigit)
import Data.List
import Data.SCC (scc)
import FlatCurry.Types
import FlatCurry.Goodies
import RW.Base
import System.IO

import Analysis.Types
import Analysis.ProgInfo
import Analysis.RootReplaced (rootCyclicAnalysis)

------------------------------------------------------------------------------
-- The termination analysis is a global function dependency analysis.
-- It assigns to a FlatCurry function definition a flag which is True
-- if this operation is terminating, i.e., whether all evaluations

terminationAnalysis :: Analysis Bool
terminationAnalysis = dependencyFuncAnalysis "Terminating" False isTerminating

-- Show termination information as a string.
showTermination :: AOutFormat -> Bool -> String
showTermination AText True  = "terminating"
showTermination ANote True  = ""
showTermination AText False = "possibly non-terminating"
showTermination ANote False = "maybe not term."

-- An operation is functionally defined if its definition is not
-- non-deterministic (no overlapping rules, no extra variables) and
-- it depends only on functionally defined operations.
isTerminating :: FuncDecl -> [(QName,Bool)] -> Bool
isTerminating (Func qfunc _ _ _ rule) calledFuncs = hasTermRule rule
 where
  hasTermRule (Rule args e) = hasTermExp (map (\a -> (a,[])) args) e
  -- we assume that all externally defined operations are terminating:
  hasTermRule (External _) = True

  hasTermExp _ (Var _)    = True
  hasTermExp _ (Lit _)    = True
  hasTermExp _ (Free _ _) = False -- could be improved if the domain is finite
  hasTermExp args (Let bs e) =
    -- compute strongly connected components of local let declarationss
    -- in order to check for recursive lets
    let sccs   = scc ((:[]) . fst) (allVars . snd) bs
    in if any (\scc -> any (`elem` concatMap allVars (map snd scc))
                           (map fst scc))
              sccs
         then False -- non-terminating due to recursive let
         else all (hasTermExp args) (e : map snd bs)
  hasTermExp args (Or e1 e2) =
    hasTermExp args e1 && hasTermExp args e2
  hasTermExp args (Case _ e bs) =
    hasTermExp args e &&
    all (\ (Branch pt be) -> hasTermExp (addSmallerArgs args e pt) be) bs
  hasTermExp args (Typed e _) = hasTermExp args e
  hasTermExp args (Comb ct qf es) =
    case ct of
      ConsCall       -> all (hasTermExp args) es
      ConsPartCall _ -> all (hasTermExp args) es
      _ -> (if qf == qfunc -- is this a recursive call?
              then any isSmallerArg (zip args es)
              else maybe False id (lookup qf calledFuncs)) &&
           all (hasTermExp args) es

  isSmallerArg ((_,sargs),exp) = case exp of
    Var v -> v `elem` sargs
    _     -> False

-- compute smaller args w.r.t. a given discriminating expression and
-- branch pattern
addSmallerArgs :: [(Int, [Int])] -> Expr -> Pattern -> [(Int, [Int])]
addSmallerArgs args de pat =
  case de of
    Var v -> maybe args
                   (\argpos -> let (av,vs) = args!!argpos
                               in replace (av, varsOf pat ++ vs) argpos args)
                   (findIndex (isInArg v) args)
    _     -> args -- other expression, no definite smaller expressions
 where
   varsOf (LPattern _)      = []
   varsOf (Pattern _ pargs) = pargs

   isInArg v (argv,svs) = v==argv || v `elem` svs

------------------------------------------------------------------------------
-- The productivity analysis is a global function dependency analysis
-- which depends on the termination analysis.
-- An operation is considered as being productive if it cannot
-- perform an infinite number of steps without producing
-- outermost constructors.
-- It assigns to a FlatCurry function definition an abstract value
-- indicating whether the function is looping or productive.

--- Data type to represent productivity status of an operation.
data Productivity =
    NoInfo
  | Terminating    -- definitely terminating operation
  | DCalls [QName] -- possible direct (top-level) calls to operations that may
                   -- not terminate, which corresponds to being productive
  | Looping        -- possibly looping
 deriving (Eq, Ord, Show, Read)

productivityAnalysis :: Analysis Productivity
productivityAnalysis =
  combinedDependencyFuncAnalysis "Productive"
                                 terminationAnalysis
                                 NoInfo
                                 isProductive

-- Show productivity information as a string.
showProductivity :: AOutFormat -> Productivity -> String
showProductivity _ NoInfo      = "no info"
showProductivity _ Terminating = "terminating"
showProductivity _ (DCalls qfs) =
  "productive / calls: " ++ "[" ++ intercalate ", " (map snd qfs) ++ "]"
showProductivity _ Looping     = "possibly looping"

lubProd :: Productivity -> Productivity -> Productivity
lubProd Looping      _            = Looping
lubProd (DCalls _ )  Looping      = Looping
lubProd (DCalls xs)  (DCalls ys)  = DCalls (sort (union xs ys))
lubProd (DCalls xs)  Terminating  = DCalls xs
lubProd (DCalls xs)  NoInfo       = DCalls xs
lubProd Terminating  p            = if p==NoInfo then Terminating else p
lubProd NoInfo       p            = p

-- An operation is productive if its recursive calls are below
-- a constructor (except for calls to terminating operations so that
-- this property is also checked).
isProductive :: ProgInfo Bool -> FuncDecl -> [(QName,Productivity)]
             -> Productivity
isProductive terminfo (Func qf _ _ _ rule) calledFuncs = hasProdRule rule
 where
  -- we assume that all externally defined operations are terminating:
  hasProdRule (External _) = Terminating
  hasProdRule (Rule _ e) =
    case hasProdExp False e of
      DCalls fs -> if qf `elem` fs then Looping else DCalls fs
      prodinfo  -> prodinfo

  -- first argument: True if we are below a constructor
  hasProdExp _  (Var _)    = Terminating
  hasProdExp _  (Lit _)    = Terminating
  hasProdExp bc (Free _ e) = -- could be improved for finite domains:
    lubProd (DCalls []) (hasProdExp bc e)
  hasProdExp bc (Let bs e) =
    -- compute strongly connected components of local let declarationss
    -- in order to check for recursive lets
    let sccs   = scc ((:[]) . fst) (allVars . snd) bs
    in if any (\scc -> any (`elem` concatMap allVars (map snd scc))
                           (map fst scc))
              sccs
         then Looping -- improve: check for variable occs under constructors
         else foldr lubProd (hasProdExp bc e)
                    (map (\ (_,be) -> hasProdExp bc be) bs)
  hasProdExp bc (Or e1 e2) = lubProd (hasProdExp bc e1) (hasProdExp bc e2)
  hasProdExp bc (Case _ e bs) =
    foldr lubProd (hasProdExp bc e)
          (map (\ (Branch _ be) -> hasProdExp bc be) bs)
  hasProdExp bc (Typed e _) = hasProdExp bc e
  hasProdExp bc (Comb ct qg es) = case ct of
    ConsCall       -> cprodargs
    ConsPartCall _ -> cprodargs
    FuncCall       -> if qg == ("Prelude","?")
                        then fprodargs -- equivalent to Or
                        else funCallInfo
    FuncPartCall _ -> funCallInfo
   where
    cprodargs = foldr lubProd NoInfo (map (hasProdExp True) es)
    fprodargs = foldr lubProd NoInfo (map (hasProdExp bc  ) es)

    funCallInfo =
      let prodinfo = if fprodargs <= Terminating
                     then if maybe False id (lookupProgInfo qg terminfo)
                            then Terminating
                            else lubProd (DCalls [qg])
                                   (maybe Looping id (lookup qg calledFuncs))
                     else Looping -- worst case assumption, could be improved...
      in if not bc then prodinfo
                   else case prodinfo of
                          DCalls _ -> DCalls []
                          _        -> prodinfo

-------------------------------------------------------------------------------
-- ReadWrite instances:

instance ReadWrite Productivity where
  readRW _ ('0' : r0) = (NoInfo,r0)
  readRW _ ('1' : r0) = (Terminating,r0)
  readRW strs ('2' : r0) = (DCalls a',r1)
    where
      (a',r1) = readRW strs r0
  readRW _ ('3' : r0) = (Looping,r0)

  showRW _ strs0 NoInfo = (strs0,showChar '0')
  showRW _ strs0 Terminating = (strs0,showChar '1')
  showRW params strs0 (DCalls a') = (strs1,showChar '2' . show1)
    where
      (strs1,show1) = showRW params strs0 a'
  showRW _ strs0 Looping = (strs0,showChar '3')

  writeRW _      h NoInfo strs = hPutChar h '0' >> return strs
  writeRW _      h Terminating strs = hPutChar h '1' >> return strs
  writeRW params h (DCalls a') strs =
    hPutChar h '2' >> writeRW params h a' strs
  writeRW _ h Looping strs = hPutChar h '3' >> return strs

  typeOf _ = monoRWType "Productivity"

------------------------------------------------------------------------------
