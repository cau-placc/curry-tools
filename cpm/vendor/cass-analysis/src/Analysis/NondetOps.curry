------------------------------------------------------------------------------
--- Nondeterminism analysis:
--- checks whether operations encapsulate or produce non-deterministic values
---
--- @author Michael Hanus
--- @version July 2024
------------------------------------------------------------------------------

module Analysis.NondetOps ( nondetOperations, showNondet, Nondet(..) )
 where

import Data.List         ( partition )

import FlatCurry.Types
import FlatCurry.Goodies ( allVars )
import RW.Base
import System.IO
import System.IO.Unsafe  ( trace )

import Analysis.Types

------------------------------------------------------------------------------
--- Data type to represent the (non-)determinism status of expressions and
--- functions.
data Nondet = Det | Nondet | FunD Nondet Nondet
 deriving (Eq, Read, Show)

-- Show determinism information as a string.
showNondet :: AOutFormat -> Nondet -> String
showNondet _ nd = showND True nd
 where
  showND _   Det          = "D"
  showND _   Nondet       = "ND"
  showND top (FunD d1 d2) = bracket top
                              (showND False d1 ++ " -> " ++ showND top d2)

  bracket top s = if top then s else "(" ++ s ++ ")"

-- Is the type purely deterministic?
-- A function is deterministic if its result is always deterministic.
isDetType :: Nondet -> Bool
isDetType Det          = True
isDetType Nondet       = False
isDetType (FunD _ nd2) = isDetType nd2

-- Least upper bound of non-determinism types.
lub :: Nondet -> Nondet -> Nondet
lub nd1 nd2 = case (nd1, nd2) of
  (Det             , Det             ) -> Det
  (Det             , Nondet          ) -> Nondet
  (Det             , FunD d1 d2      ) -> FunD d1 d2
  (Nondet          , _               ) -> Nondet
  (FunD _ _        , Det             ) -> if isDetType nd1 then nd1 else Nondet
  (FunD _ _        , Nondet          ) -> Nondet
  (FunD Det df1    , FunD Det df2    ) -> FunD Det    (lub df1 df2)
  (FunD Nondet df1 , FunD Nondet df2 ) -> FunD Nondet (lub df1 df2)
  (FunD Nondet df1 , FunD Det    df2 ) -> FunD Det    (lub df1 df2)
  (FunD Det df1    , FunD Nondet df2 ) -> FunD Det    (lub df1 df2)
  _ -> trace ("Warning: lub of '" ++ show nd1 ++ "' and '" ++
              show nd2 ++ "' not covered!") Nondet

--- Non-determinism type analysis.
nondetOperations :: Analysis Nondet
nondetOperations = dependencyFuncAnalysis "NonDetOps" Det ndFunc

ndFunc :: FuncDecl -> [(QName,Nondet)] -> Nondet
ndFunc (Func (m,f) ar _ _ (External _)) _ =
 maybe (genDetFun ar) -- external operations are deterministic
       id
       (lookup (m,f) stdNondetTypes)
ndFunc (Func (m,f) _ _ _ (Rule args rhs)) calledFuncs =
 maybe (ndFuncRule calledFuncs (m,f) args rhs)
       id
       (lookup (m,f) stdNondetTypes)

-- Non-determinism types for some standard operations.
stdNondetTypes :: [(QName,Nondet)]
stdNondetTypes =
  map (\ (f,nd) -> ((prelude,f), nd))
      [ ("apply"  , FunD (FunD Det Det) (FunD Det Det) )
      , ("?"      , FunD Det (FunD Det Nondet) )
      , ("unknown", FunD (FunD Det Det) Nondet )
      ] ++
  map (\ (f,nd) -> (("Control.AllValues",f), nd)) allValOps ++
  map (\ (f,nd) -> (("Control.Findall"  ,f), nd)) allValOps
 where
  allValOps =
    [ ("allSolutions", FunD Det (FunD Nondet Det))
    , ("allValues"   , FunD Nondet Det)
    , ("oneSolution" , FunD Det (FunD Nondet Det))
    , ("oneValue"    , FunD Nondet Det)
    , ("someSolution", FunD Det (FunD Nondet Det))
    , ("someValue"   , FunD Nondet Det)
    , ("isFail"      , FunD Nondet Det)
    , ("rewriteAll"  , FunD Nondet Det)
    , ("rewriteSome" , FunD Nondet Det)
    ]

-- Generate a deterministic operation type of a given arity.
genDetFun :: Int -> Nondet
genDetFun n = if n==0 then Det else FunD Det (genDetFun (n-1))

-- Generate a non-deterministic operation type of a given arity.
genNondetFun :: Int -> Nondet
genNondetFun n = if n==0 then Nondet else FunD Det (genNondetFun (n-1))

-- Generate a `Nondet` type from an argument environment and a result type.
genArgEnvFun :: [(Int,Nondet)] -> Nondet -> Nondet
genArgEnvFun []               ndr = ndr
genArgEnvFun ((_,nda):argenv) ndr = FunD nda (genArgEnvFun argenv ndr)

-- Analyze an operation defined by a rule.
ndFuncRule :: [(QName,Nondet)] -> QName -> [Int] -> Expr -> Nondet
ndFuncRule calledFuncs qf args rhs = tryNondetArg [] args
 where
  tryNondetArg argsenv []     = genArgEnvFun argsenv (ndExp argsenv rhs)
  tryNondetArg argsenv (i:is) =
    -- Try whether argument i as ND yields D.
    -- If yes, set it to ND, otherwise to D.
    let ndres = ndExp (argsenv ++ [(i,Nondet)] ++ map (\j -> (j,Det)) is) rhs
        argnd = if ndres == Det then Nondet else Det
    in tryNondetArg (argsenv ++ [(i,argnd)]) is

  applyToAbsValue g absfun =
    maybe (error $ "Abstract value of " ++ show g ++ " not found!")
          absfun
          (lookup g calledFuncs)

  ndExp env (Var i)        = maybe (trace (show (snd qf) ++ ": variable " ++
                                           show i ++ " undefined!") Nondet)
                                   id
                                   (lookup i env)
  ndExp _   (Lit _)        = Det
  ndExp env (Comb ct g es) = case ct of
    FuncCall       -> ndFuncCall g 0 ndargs
    FuncPartCall m -> ndFuncCall g m ndargs
    ConsPartCall m -> if all isDetType ndargs then genDetFun m
                                              else genNondetFun m
    ConsCall       -> if all isDetType ndargs then Det else Nondet
   where ndargs = map (ndExp env) es
  ndExp env (Free vs e)    = ndExp (map (\i -> (i,Nondet)) vs ++ env) e
  ndExp env (Let bs e)     = ndLet env bs e
  ndExp _   (Or _ _)       = Nondet
  ndExp env (Case _  e bs)
    | ndExp env e == Nondet = Nondet
    | otherwise             = foldr lub Det (map ndBranch bs)
   where
    ndBranch (Branch (Pattern _ vs) be) = ndExp (map (\i -> (i,Det)) vs ++ env)
                                                be
    ndBranch (Branch (LPattern _)   be) = ndExp env be
  ndExp env (Typed e _)    = ndExp env e

  ndFuncCall g m ndargs
    | g == (prelude, "apply")
    = case ndargs of
        [FunD Nondet Det, _] -> Det -- specific case not covered by std ND type
        _                    -> applyToAbsValue g (matchArgs qf g m ndargs)
    | isSetCombinator g
    = ndSetFunc m ndargs
    | otherwise
    = applyToAbsValue g (matchArgs qf g m ndargs)

  ndSetFunc _ [] = error $ "Illegal use of set function in '" ++ snd qf ++ "'!"
  ndSetFunc m (ndfunc:ndargs) =
    matchArgs qf ("??","??") m ndargs (setResultD ndfunc)

  -- Analyse let expressions. For recursive bindings, analyse all these
  -- bindings where `Det` is assumed as the initial value for the variables.
  -- If some analysis returns `Nondet`, change the assumption to `Nondet`.
  ndLet env []          e = ndExp env e
  ndLet env ((i,be):bs) e =
    if all (`notElem` (i : map fst bs)) (allVars be)
      then -- no recursive binding:
           ndLet ((i, ndExp env be) : env) bs e
      else -- recursive binding: find binding group and try `Det` or `Nondet`:
           ndLetGroup env [] [(i,be)] bs e

  ndLetGroup env recbs []           allbs e =
    -- Analyze the first recursive binding group and proceed with
    -- an appropriately extended environment:
    let vardetenv    = map (\ (i,_) -> (i,Det)  ) recbs ++ env
        varnondetenv = map (\ (i,_) -> (i,Nondet)) recbs ++ env
        nds = map (ndExp vardetenv) (map snd recbs)
    in if all (== Det) nds
         then ndLet vardetenv    allbs e
         else ndLet varnondetenv allbs e
  ndLetGroup env grpbs ((i,be):rbs) allbs e =
    let (recbs, otherbs) = selectBindings (allVars be) allbs
    in ndLetGroup env (grpbs ++ [(i,be)]) (rbs ++ recbs) otherbs e

  -- Select in a list of bindings all recursive bindings of the given variables
  -- and return them together with the remaining ones:
  selectBindings []     bs = ([], bs)
  selectBindings (v:vs) bs =
    let (vbs,obs) = partition ((==v) . fst) bs
        (rbs,nbs) = selectBindings vs obs
    in  (vbs ++ rbs, nbs)


-- Match the actual arguments against a non-deterministic operation type.
matchArgs :: QName -> QName -> Int -> [Nondet] -> Nondet -> Nondet
matchArgs f g misargs ndofargs ndoffunc = matchA ndofargs ndoffunc
 where
  matchA []           nd = nd
  matchA (nd1:ndargs) nd = case nd of
    Det -> -- possibly unknown ND status:
           if all isDetType (nd1:ndargs) then genDetFun misargs
                                         else genNondetFun misargs
    _   -> case nd1 of
             Det -> case nd of 
               FunD _ ndfunc -> matchA ndargs ndfunc
               _             -> noMatchWarning
             Nondet -> case nd of
               FunD Nondet         ndfunc -> matchA ndargs ndfunc
               FunD Det            ndfunc -> matchA ndargs (setResultND ndfunc)
               FunD (FunD Det Det) ndfunc -> matchA ndargs (setResultND ndfunc)
               _                          -> noMatchWarning
             FunD _ _ -> case nd of
               FunD ndf1 ndfunc -> if isDetType nd1
                                     then matchA ndargs ndfunc
                                     else if ndf1 == Nondet
                                            then matchA ndargs ndfunc
                                            else matchA ndargs
                                                        (setResultND ndfunc)
               _                -> noMatchWarning

  noMatchWarning =
    trace ("Warning: matchArgs '" ++ show ndofargs ++ "' against '" ++
           show ndoffunc ++ "' not covered in application of " ++
           show g ++ " in function '" ++ snd f ++ "'!") Nondet

-- Sets the result type (of a function) to `Nondet`.
setResultND :: Nondet -> Nondet
setResultND nd = case nd of FunD _ ndr -> FunD Det (setResultND ndr)
                            _          -> Nondet


-- Sets the result type (of a function) to `Det`.
setResultD :: Nondet -> Nondet
setResultD nd = case nd of FunD _ ndr -> FunD Det (setResultD ndr)
                           _          -> Det

prelude :: String
prelude = "Prelude"

-- Name of a set function combinator?
isSetCombinator :: QName -> Bool
isSetCombinator (mn,fn) = mn == "Control.SetFunctions" && take 3 fn == "set"

------------------------------------------------------------------------------
-- ReadWrite instances:

instance ReadWrite Nondet where
  readRW strs ('0' : r0) = (Det,r0)
  readRW strs ('1' : r0) = (Nondet,r0)
  readRW strs ('2' : r0) = (FunD a' b',r2)
    where
      (a',r1) = readRW strs r0
      (b',r2) = readRW strs r1

  showRW params strs0 Det = (strs0,showChar '0')
  showRW params strs0 Nondet = (strs0,showChar '1')
  showRW params strs0 (FunD a' b') = (strs2,showChar '2' . (show1 . show2))
    where
      (strs1,show1) = showRW params strs0 a'
      (strs2,show2) = showRW params strs1 b'

  writeRW params h Det strs = hPutChar h '0' >> return strs
  writeRW params h Nondet strs = hPutChar h '1' >> return strs
  writeRW params h (FunD a' b') strs =
    hPutChar h '2' >> (writeRW params h a' strs >>= writeRW params h b')

  typeOf _ = monoRWType "Nondet"

------------------------------------------------------------------------------
