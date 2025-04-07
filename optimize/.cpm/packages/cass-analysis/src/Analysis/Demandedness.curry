------------------------------------------------------------------------------
--- Demandedness analysis:
--- checks whether functions demands a particular argument, i.e.,
--- delivers only bottom if some argument is bottom.
---
--- @author Michael Hanus
--- @version February 2025
------------------------------------------------------------------------------

module Analysis.Demandedness
 where

import Data.List         ( (\\), intercalate )

import FlatCurry.Types
import FlatCurry.Goodies

import Analysis.Types

------------------------------------------------------------------------------
--- Data type to represent information about demanded arguments.
--- Demanded arguments are represented as a list of indices
--- for the arguments, where arguments are numbered from 1.
type DemandedArgs = [Int]

-- Show determinism information as a string.
showDemand :: AOutFormat -> DemandedArgs -> String
showDemand AText []     = "no demanded arguments"
showDemand ANote []     = ""
showDemand fmt (x:xs) =
  (if fmt==AText then "demanded arguments: " else "") ++
  intercalate "," (map show (x:xs))

-- Abstract demand domain.
data DemandDomain = Bot | Top
  deriving Eq

-- Least upper bound on abstract demand domain.
lub :: DemandDomain -> DemandDomain -> DemandDomain
lub Bot x = x
lub Top _ = Top

--- Demandedness analysis.
demandAnalysis :: Analysis DemandedArgs
demandAnalysis = dependencyFuncAnalysis "Demand" [1..] daFunc

-- We define the demanded arguments of some primitive prelude operations.
-- Otherwise, we analyse the right-hand sides of the rule.
daFunc :: FuncDecl -> [(QName,DemandedArgs)] -> DemandedArgs
daFunc (Func (m,f) _ _ _ rule) calledFuncs
 | m == prelude && f `elem` prelude2s = [1,2]
 | m == prelude && f `elem` prelude1s = [1]
 | otherwise = daFuncRule calledFuncs rule
 where
  prelude2s = ["==","=:=","compare","<=","$#","$##","$!","$!!",
               "+","-","*","div","mod","divMod","quot","rem","quotRem"]
  prelude1s = ["seq","ensureNotFree","apply","cond","=:<=","negateFloat"]
 -- TODO: >>= catch catchFail


daFuncRule :: [(QName,DemandedArgs)] -> Rule -> DemandedArgs
daFuncRule _           (External _)    = [] -- nothing known about externals
daFuncRule calledFuncs (Rule args rhs) =
  map fst
      (filter ((==Bot) . snd)
              (map (\botarg -> (botarg, absEvalExpr rhs [botarg]))
                   args))
 where
  -- abstract evaluation of an expression w.r.t. variables assumed to be Bot
  absEvalExpr (Var i)        bvs = if i `elem` bvs then Bot else Top
  absEvalExpr (Lit _)        _   = Top
  absEvalExpr (Comb ct g es) bvs
    | ct == FuncCall
    = if g == (prelude,"failed")
        then Bot -- Prelude.failed never returns a value
        else maybe (error $ "Abstract value of " ++ show g ++ " not found!")
                   (\gdas -> let curargs = map (\(i,e) -> (i,absEvalExpr e bvs))
                                               (zip [1..] es)
                                 cdas = gdas \\
                                        map fst (filter ((/=Bot) . snd) curargs)
                             in if null cdas then Top else Bot)
                   (lookup g calledFuncs)
    | otherwise = Top
  absEvalExpr (Free _ e)     bvs = absEvalExpr e bvs
  absEvalExpr (Let bs e)     bvs = absEvalExpr e (absEvalBindings bs bvs)
  absEvalExpr (Or e1 e2)     bvs = lub (absEvalExpr e1 bvs) (absEvalExpr e2 bvs)
  absEvalExpr (Case _  e bs) bvs =
    if absEvalExpr e bvs == Bot
      then Bot
      else foldr lub Bot (map absEvalBranch bs)
   where absEvalBranch (Branch _ be) = absEvalExpr be bvs
  absEvalExpr (Typed e _) bvs = absEvalExpr e bvs

    -- could be improved with local fixpoint computation
  absEvalBindings []             bvs = bvs
  absEvalBindings ((i,exp) : bs) bvs =
    let ival = absEvalExpr exp bvs
    in if ival==Bot
         then absEvalBindings bs (i:bvs)
         else absEvalBindings bs bvs

prelude :: String
prelude = "Prelude"

------------------------------------------------------------------------------
