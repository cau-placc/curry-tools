------------------------------------------------------------------------------
--- Residuation analysis:
--- checks whether a function does not residuate and yields a ground value
--- if some arguments are ground
---
--- @author Michael Hanus
--- @version September 2018
------------------------------------------------------------------------------

module Analysis.Residuation
  ( ResiduationInfo(..), residuationAnalysis, showResInfo )
 where

import Data.List ( intercalate, union )

import Analysis.Types
import FlatCurry.Types
import FlatCurry.Goodies

------------------------------------------------------------------------------
--- Data type to represent residuation information.
--- If an operation has info `MayResiduate`, it may residuate
--- or yields a non-ground value even if all arguments are ground.
--- If an operation has info `NoResiduateIf xs`, it does not residuate
--- and yields a ground value if all arguments in the index list are ground,
--- where arguments are numbered from 1.
data ResiduationInfo = MayResiduate
                     | NoResiduateIf [Int]
                     | NoResInfo
  deriving (Show, Read, Eq)

-- Least upper bound of residuation information
lubNRI :: ResiduationInfo -> ResiduationInfo -> ResiduationInfo
lubNRI MayResiduate _ = MayResiduate
lubNRI NoResInfo    nri = nri
lubNRI (NoResiduateIf _ ) MayResiduate = MayResiduate
lubNRI (NoResiduateIf xs) NoResInfo    = NoResiduateIf xs
lubNRI (NoResiduateIf xs) (NoResiduateIf ys) = NoResiduateIf (unionS xs ys)

-- union on sorted lists:
unionS :: Ord a => [a] -> [a] -> [a]
unionS []     ys     = ys
unionS (x:xs) []     = x:xs
unionS (x:xs) (y:ys) | x==y = x : unionS xs ys
                     | x<y  = x : unionS xs (y:ys)
                     | x>y  = y : unionS (x:xs) ys

-- Show non-residuation information as a string.
showResInfo :: AOutFormat -> ResiduationInfo -> String
showResInfo AText MayResiduate = "may residuate or has non-ground result"
showResInfo ANote MayResiduate = "residuate"
showResInfo AText (NoResiduateIf xs) =
  "does not residuate" ++
  case xs of
    []  -> ""
    [x] -> " if argument " ++ show x ++ " is ground"
    _   -> " if arguments " ++ intercalate "," (map show xs) ++ " are ground"
showResInfo ANote (NoResiduateIf xs) =
  "non-residuating" ++
  if null xs then "" else " if " ++ intercalate "," (map show xs)
showResInfo AText NoResInfo = "unknown residuation behavior"
showResInfo ANote NoResInfo = "???"

--- Non-residuation analysis.
residuationAnalysis :: Analysis ResiduationInfo
residuationAnalysis = dependencyFuncAnalysis "Residuation" NoResInfo nrFunc

-- We define the demanded arguments of some primitive prelude operations.
-- Otherwise, we analyse the right-hand sides of the rule.
nrFunc :: FuncDecl -> [(QName,ResiduationInfo)] -> ResiduationInfo
nrFunc (Func fn ar _ _ rule) calledFuncs = nrFuncRule fn ar calledFuncs rule

nrFuncRule :: QName -> Int -> [(QName,ResiduationInfo)] -> Rule
           -> ResiduationInfo
-- We assume that all external operations do not residuate if all
-- arguments are non-residuating and ground.
-- This is true for all known standard external operations.
-- If this does not hold for some unusual operation,
-- it must be specified here.
nrFuncRule _ farity _ (External _) = NoResiduateIf [1 .. farity]

nrFuncRule _ _ calledFuncs (Rule args rhs) =
  nrExp (map (\i -> (i, NoResiduateIf [i])) args) rhs
 where
  -- Analyze residuation behavior of an expression.
  -- The first argument maps variables to their non-residuating conditions
  -- if these variables are used in an expression.
  nrExp _    (Lit _) = NoResiduateIf []
  nrExp amap (Var i) = maybe MayResiduate id (lookup i amap)

  nrExp amap (Comb ct g es) = case ct of
    FuncCall       -> maybe NoResInfo checkNonResArgs     (lookup g calledFuncs)
    FuncPartCall _ -> maybe NoResInfo checkNonResPartArgs (lookup g calledFuncs)
    _ -> if null es
           then NoResiduateIf []
           else foldr1 lubNRI (map (nrExp amap) es)
   where
    checkNonResArgs NoResInfo          = NoResInfo
    checkNonResArgs MayResiduate       = MayResiduate
    checkNonResArgs (NoResiduateIf xs) =
      if null xs
        then NoResiduateIf []
        else foldr1 lubNRI (map (\i -> nrExp amap (es!!(i-1))) xs)

    checkNonResPartArgs NoResInfo          = NoResInfo
    checkNonResPartArgs MayResiduate       = MayResiduate
    checkNonResPartArgs (NoResiduateIf xs) =
      let pxs = filter (<= length es) xs
      in if null pxs
           then NoResiduateIf []
           else foldr1 lubNRI (map (\i -> nrExp amap (es!!(i-1))) pxs)

  nrExp amap (Case _ e bs) = foldr lubNRI nrcexp (map nrBranch bs)
   where
    nrcexp = nrExp amap e -- non-res. condition of discriminating expression

    nrBranch (Branch (LPattern _) be) = nrExp amap be
    nrBranch (Branch (Pattern _ xs) be) =
      nrExp (map (\x -> (x,nrcexp)) xs ++ amap) be

  nrExp amap (Free _ e)  = nrExp amap e

  -- could be improved by sorting bindings by their variable dependencies
  -- (which seems already done by the front-end)
  nrExp amap (Let bindings e)  =
    -- initialize all bound variables with `NoResInfo` which is meaningful
    -- for recursive bindings:
    let initamap = map (\ (v,_) -> (v,NoResInfo)) bindings ++ amap
    in nrExp (addBindings initamap bindings) e
   where
    addBindings amp []          = amp
    addBindings amp ((v,be):bs) = addBindings ((v, nrExp amp be) : amp) bs

  nrExp amap (Or e1 e2)  = lubNRI (nrExp amap e1) (nrExp amap e2)

  nrExp amap (Typed e _) = nrExp amap e

prelude :: String
prelude = "Prelude"

------------------------------------------------------------------------------
