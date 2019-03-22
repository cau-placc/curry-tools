------------------------------------------------------------------------
--- Groundness/non-determinism effect analysis based on
--- [Brassel/Hanus'05](http://www.informatik.uni-kiel.de/~mh/papers/ICLP05.html).
---
--- @author Michael Hanus
--- @version May 2013
------------------------------------------------------------------------

module Analysis.Groundness
  ( Ground(..), showGround, groundAnalysis
  , NDEffect(..), showNDEffect, ndEffectAnalysis
  ) where

import FlatCurry.Types
import Data.List

import Analysis.Types
import Analysis.ProgInfo

------------------------------------------------------------------------
-- Analyze the groundness of functions.
------------------------------------------------------------------------

--- Type to represent groundness information.
--- Definitely ground (G), maybe non-ground (A), or maybe non-ground
--- if i-th argument is non-ground (P [...,i,...]).
data Ground = G | A | P [Int]
 deriving Eq

-- Show groundness information as a string.
showGround :: AOutFormat -> Ground -> String
showGround ANote G      = "G"
showGround AText G      = "always ground result"
showGround ANote A      = "A"
showGround AText A      = "possibly non-ground result"
showGround ANote (P ps) = show ps
showGround AText (P ps) =
  "ground if argument" ++
  (if length ps == 1 then ' ' : show (head ps) ++ " is ground"
                     else "s " ++ show ps ++ " are ground")

-- Lowest upper bound on groundness information.
lubG :: Ground -> Ground -> Ground
lubG G      y      = y
lubG A      _      = A
lubG (P ps) G      = P ps
lubG (P _ ) A      = A
lubG (P ps) (P qs) = P (mergeInts ps qs)

------------------------------------------------------------------------
-- Analyze the groundness information of functions.

groundAnalysis :: Analysis Ground
groundAnalysis = dependencyFuncAnalysis "Groundness" G groundFunc

groundFunc :: FuncDecl -> [(QName,Ground)] -> Ground
groundFunc (Func (m,f) _ _ _ rule) calledFuncs
 | m==prelude && f `elem` preludeGroundFuncs = G
 | m==prelude = maybe anaresult id (lookup f preludeFuncs)
 | otherwise  = anaresult
 where
  anaresult = groundFuncRule calledFuncs rule

  preludeFuncs = [("cond",P [2]),("seq",P [2]),("ensureNotFree",P [1])]

  preludeGroundFuncs =
    ["+","-","*","div","mod","divMod","quot","rem","quotRem","negateFloat",
     "==","=:=","=:<=","compare","<",">","<=",">=","failed","error"]


groundFuncRule :: [(QName,Ground)] -> Rule -> Ground
groundFuncRule _ (External _) = A -- nothing known about other externals
groundFuncRule calledFuncs (Rule args rhs) =
  absEvalExpr (zip args (map (\i->P [i]) [1..])) rhs
 where
  -- abstract evaluation of an expression w.r.t. groundness environment
  absEvalExpr env (Var i)  = maybe A -- occurs in case of recursive lets
                                   id  (lookup i env)
  absEvalExpr _   (Lit _)  = G
  absEvalExpr env (Comb ct g es) =
    if ct == FuncCall
    then maybe (error $ "Abstract value of " ++ show g ++ " not found!")
               (\gd -> let curargs = zip [1..] (map (absEvalExpr env) es)
                        in groundApply gd curargs)
               (lookup g calledFuncs)
    else foldr lubG G (map (absEvalExpr env) es)
  absEvalExpr env (Free vs e) = absEvalExpr (zip vs (repeat A) ++ env) e
  absEvalExpr env (Let bs e)  = absEvalExpr (absEvalBindings env bs) e
  absEvalExpr env (Or e1 e2)  = lubG (absEvalExpr env e1) (absEvalExpr env e2)
  absEvalExpr env (Typed e _) = absEvalExpr env e
  absEvalExpr env (Case _  e bs) = foldr lubG G (map absEvalBranch bs)
   where
    gcase = absEvalExpr env e

    absEvalBranch (Branch (LPattern _) be) = absEvalExpr env be
    absEvalBranch (Branch (Pattern _ pargs) be) =
      absEvalExpr (map (\pi -> (pi,gcase)) pargs ++ env) be

  -- could be improved for recursive lets with local fixpoint computation
  absEvalBindings env [] = env
  absEvalBindings env ((i,exp):bs) =
    absEvalBindings ((i, absEvalExpr env exp) : env) bs

-- compute groundness information for an application
groundApply :: Ground -> [(Int,Ground)] -> Ground
groundApply G _ = G
groundApply A _ = A
groundApply (P ps) gargs =
  foldr lubG G (map (\p -> maybe A id (lookup p gargs)) ps)


-----------------------------------------------------------------------
-- Non-determinism effect analysis
-----------------------------------------------------------------------

--- Type to represent non-determinism effects.
--- A non-determinism effect can be due to an Or (first argument),
--- due to a narrowing step (second argument), or if i-th argument
--- is non-ground (if i is a member of the third argument).
data NDEffect = NDEffect Bool Bool [Int]
  deriving Eq

noEffect :: NDEffect
noEffect = NDEffect False False []

orEffect :: NDEffect
orEffect = NDEffect True False []

narrEffect :: NDEffect
narrEffect = NDEffect False True []

narrIfEffect :: [Int] -> NDEffect
narrIfEffect = NDEffect False False

-- Show non-determinitic effect information as a string.
showNDEffect :: AOutFormat -> NDEffect -> String
showNDEffect ANote (NDEffect ornd narr ifs) = intercalate " " $
  (if ornd then ["choice"] else []) ++
  (if narr then ["narr"]   else []) ++
  (if not (null ifs) then ["narrIf"++show ifs] else [])
showNDEffect AText (NDEffect ornd narr ifs) = intercalate " / " $
  (if ornd then ["choice"] else []) ++
  (if narr then ["possibly non-deterministic narrowing steps"] else []) ++
  (if not (null ifs)
   then ["non-deterministic narrowing if argument" ++
         (if length ifs == 1 then ' ' : show (head ifs) ++ " is non-ground"
                            else "s " ++ show ifs ++ " are non-ground")]
   else [])

-- Lowest upper bound on non-determinism effects.
lubE :: NDEffect -> NDEffect -> NDEffect
lubE (NDEffect ornd1 narr1 ifs1) (NDEffect ornd2 narr2 ifs2) =
  NDEffect (ornd1 || ornd2) narr (if narr then [] else mergeInts ifs1 ifs2)
 where
   narr = narr1 || narr2

-- Lowest upper bound on groundness/non-determinism effects.
lubGE :: (Ground,NDEffect) -> (Ground,NDEffect) -> (Ground,NDEffect)
lubGE (g1,ne1) (g2,ne2) = (lubG g1 g2, lubE ne1 ne2)

------------------------------------------------------------------------
-- Analyze the non-determinism effect of functions.

ndEffectAnalysis :: Analysis NDEffect
ndEffectAnalysis =
  combinedDependencyFuncAnalysis "NDEffect" groundAnalysis noEffect ndEffectFunc

ndEffectFunc :: ProgInfo Ground -> FuncDecl -> [(QName,NDEffect)] -> NDEffect
ndEffectFunc groundinfo (Func (m,f) _ _ _ rule) calledFuncs
 | m==prelude = maybe anaresult id (lookup f preludeFuncs)
 | otherwise  = anaresult
 where
  anaresult = ndEffectFuncRule groundinfo calledFuncs rule

  preludeFuncs = [("?",orEffect)]


ndEffectFuncRule :: ProgInfo Ground -> [(QName,NDEffect)] -> Rule -> NDEffect
ndEffectFuncRule _ _ (External _) = noEffect -- externals are deterministic
ndEffectFuncRule groundinfo calledFuncs (Rule args rhs) =
  snd (absEvalExpr (zip args (map (\i->(P [i],noEffect)) [1..])) rhs)
 where
  -- abstract evaluation of an expression w.r.t. NDEffect environment
  absEvalExpr env (Var i)  = maybe (A,noEffect) id (lookup i env)
  absEvalExpr _   (Lit _)  = (G,noEffect)
  absEvalExpr env (Comb ct g es) =
    if ct == FuncCall
    then maybe (error $ "Abstract value of " ++ show g ++ " not found!")
               (\gnd -> let curargs = zip [1..] (map (absEvalExpr env) es) in
                   maybe (error $ "Ground value of " ++ show g ++ " not found!")
                         (\ggd -> ndEffectApply (ggd,gnd) curargs)
                         (lookupProgInfo g groundinfo))
               (lookup g calledFuncs)
    else foldr lubGE (G,noEffect) (map (absEvalExpr env) es)
  absEvalExpr env (Free vs e) =
    absEvalExpr (zip vs (repeat (A,noEffect)) ++ env) e
  absEvalExpr env (Let bs e)  = absEvalExpr (absEvalBindings env bs) e
  absEvalExpr env (Or e1 e2)  =
    let (g1,nd1) = absEvalExpr env e1
        (g2,nd2) = absEvalExpr env e2
     in (lubG g1 g2, lubE orEffect (lubE nd1 nd2))
  absEvalExpr env (Typed e _) = absEvalExpr env e
  absEvalExpr env (Case ctype e bs) =
    if ctype==Rigid {- not really for KiCS2 -} || gcase==G || length bs == 1
    then (gbrs, lubE ndbrs ndcase)
    else (gbrs, lubE (ground2nondet gcase) (lubE ndbrs ndcase))
   where
    (gcase,ndcase) = absEvalExpr env e

    (gbrs,ndbrs) = foldr lubGE (G,noEffect) (map absEvalBranch bs)

    ground2nondet G      = noEffect
    ground2nondet A      = narrEffect
    ground2nondet (P ps) = narrIfEffect ps

    absEvalBranch (Branch (LPattern _) be) = absEvalExpr env be
    absEvalBranch (Branch (Pattern _ pargs) be) =
      absEvalExpr (map (\pi -> (pi,(gcase,noEffect))) pargs ++ env) be

  -- could be improved for recursive lets with local fixpoint computation
  absEvalBindings env [] = env
  absEvalBindings env ((i,exp):bs) =
    absEvalBindings ((i, absEvalExpr env exp) : env) bs

-- compute ground/nondet effect information for an application
ndEffectApply :: (Ground,NDEffect) -> [(Int,(Ground,NDEffect))]
              -> (Ground,NDEffect)
ndEffectApply (fgd,fnd) argsgnd =
  let (argsgd,argsnd) = unzip (map (\ (i,(gd,nd)) -> ((i,gd),nd)) argsgnd)
   in (groundApply fgd argsgd,
       foldr lubE (ndEffectReplace argsgd fnd) argsnd)

-- replace (narrIf i) by i-th ground value
ndEffectReplace :: [(Int,Ground)] -> NDEffect -> NDEffect
ndEffectReplace argsgd (NDEffect ornd narrnd ifs) = replaceProjs [] ifs
 where
  -- replace i by i-th ground value
  replaceProjs ps [] = NDEffect ornd narrnd ps
  replaceProjs ps (i:is) =
    maybe (error $ "Ground value of argument " ++ show i ++ " not found!")
          (\g -> case g of G     -> replaceProjs ps is
                           A     -> NDEffect ornd True []
                           P ips -> replaceProjs (mergeInts ips ps) is)
          (lookup i argsgd)

-----------------------------------------------------------------------
-- merge ascending lists of integers and remove duplicates
mergeInts :: [Int] -> [Int] -> [Int]
mergeInts []     ys = ys
mergeInts (x:xs) [] = x:xs
mergeInts (x:xs) (y:ys) | x==y = x : mergeInts xs ys
                        | x<y  = x : mergeInts xs (y:ys)
                        | x>y  = y : mergeInts (x:xs) ys

prelude :: String
prelude = "Prelude"

-----------------------------------------------------------------------
