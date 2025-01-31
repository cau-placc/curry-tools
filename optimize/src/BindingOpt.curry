-------------------------------------------------------------------------
--- Implementation of a transformation to replace Boolean equalities
--- by equational constraints (which binds variables).
---
--- @author Michael Hanus
--- @version October 2024
-------------------------------------------------------------------------

module BindingOpt (main, transformFlatProg) where

import Control.Monad               ( when, unless )
import Curry.Compiler.Distribution ( installDir, curryCompiler )
import Data.List
import Data.Maybe                  ( fromJust, isJust )
import System.Environment          ( getArgs )
import System.CPUTime              ( getCPUTime )

import FlatCurry.Types hiding  (Cons)
import FlatCurry.Files
import FlatCurry.Goodies
import RW.Base             ( ReadWrite )
import System.CurryPath    ( runModuleAction )
import System.Directory    ( renameFile )
import System.FilePath     ( (</>), (<.>), normalise, pathSeparator
                           , takeExtension, dropExtension )
import System.Process      ( system, exitWith )

import Analysis.Types
import Analysis.ProgInfo
import Analysis.RequiredValues
import CASS.Server       ( analyzeGeneric, analyzePublic, analyzeInterface )
import System.CurryPath  ( currySubdir, addCurrySubdir, splitModuleFileName )
import Text.CSV

------------------------------------------------------------------------------
-- The options for the transformation.
data Options = Options { verbosity    :: Int   -- verbosity
                       , withAnalysis :: Bool  -- use analysis?
                       , eqvTrans     :: Bool  -- transform also (==)?
                       , loadProg     :: Bool  -- load transformed program?
                       }

defaultOptions :: Options
defaultOptions = Options 1 True True False

systemBanner :: String
systemBanner =
  let bannerText = "Curry Binding Optimizer (version of 07/01/2021)"
      bannerLine = take (length bannerText) (repeat '=')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine

usageComment :: String
usageComment = unlines
  [ "Usage: curry-transbooleq [option] ... [module or FlatCurry file] ..."
  , "       -v<n>  : set verbosity level (n=0|1|2|3)"
  , "       -f     : fast transformation without analysis"
  , "                (uses only information about the standard prelude)"
  , "       -s     : transform only (===) but not (==)"
  , "       -l     : load optimized module into Curry system"
  , "       -h, -? : show this help text"
  ]

------------------------------------------------------------------------------

-- main function to call the optimizer:
main :: IO ()
main = getArgs >>= checkArgs defaultOptions

mainCallError :: [String] -> IO ()
mainCallError args = do
  putStrLn $ systemBanner
    ++ "\nIllegal arguments: " ++ unwords args
    ++ "\n" ++ usageComment
  exitWith 1

checkArgs :: Options -> [String] -> IO ()
checkArgs opts args = case args of
  []                   -> mainCallError []
  ('-':'v':d:[]):margs -> let v = ord d - ord '0'
                          in if v >= 0 && v < 4
                               then checkArgs opts { verbosity = v } margs
                               else mainCallError args
  "-f" : margs         -> checkArgs opts { withAnalysis = False } margs
  "-s" : margs         -> checkArgs opts { eqvTrans = False } margs
  "-l" : margs         -> checkArgs opts { loadProg = True  } margs
  "-h" : _             -> putStr (systemBanner++'\n':usageComment)
  "-?" : _             -> putStr (systemBanner++'\n':usageComment)
  mods                 -> do printVerbose opts 1 systemBanner
                             mapM_ (transformBoolEq opts) mods

-- Verbosity level:
-- 0 : show nothing
-- 1 : show summary of optimizations performed
-- 2 : show analysis infos and details of optimizations including timings
-- 3 : show analysis infos also of imported modules

-- Output a string w.r.t. verbosity level
printVerbose :: Options -> Int -> String -> IO ()
printVerbose opts printlevel message =
  unless (null message || verbosity opts < printlevel) $ putStrLn message

transformBoolEq :: Options -> String -> IO ()
transformBoolEq opts name = do
  if takeExtension name == ".fcy"
    then do prog <- readFlatCurryFile name
            let modname = modNameOfFcyName (normalise (dropExtension name))
            transformAndStoreFlatProg opts modname name prog
    else runModuleAction
           (\mn -> readFlatCurry mn >>=
                   transformAndStoreFlatProg opts mn (flatCurryFileName mn))
           name

-- Extracts the module name from a given FlatCurry file name:
modNameOfFcyName :: String -> String
modNameOfFcyName name =
  let wosuffix = normalise (dropExtension name)
      [dir,wosubdir] = splitOn (currySubdir ++ [pathSeparator]) wosuffix
   in -- construct hierarchical module name:
      dir </> intercalate "." (split (==pathSeparator) wosubdir)

transformAndStoreFlatProg :: Options -> String -> String -> Prog -> IO ()
transformAndStoreFlatProg opts modname fcyfile prog = do
  printVerbose opts 1 $ "Reading and analyzing module '" ++ modname ++ "'..."
  starttime <- getCPUTime
  (newprog, transformed) <- transformFlatProg opts modname prog
  let optfcyfile = fcyfile ++ "_OPT"
  when transformed $ writeFCY optfcyfile newprog
  stoptime <- getCPUTime
  printVerbose opts 2 $ "Transformation time for " ++ modname ++ ": " ++
                        show (stoptime-starttime) ++ " msecs"
  when transformed $ do
    printVerbose opts 2 $ "Transformed program stored in " ++ optfcyfile
    renameFile optfcyfile fcyfile
    printVerbose opts 2 $ " ...and moved to " ++ fcyfile
  when (loadProg opts) $ do
    system $ curryComp ++ " -Dbindingoptimization=no :l " ++ modname
    return ()
 where curryComp = installDir </> "bin" </> curryCompiler

-- Perform the binding optimization on a FlatCurry program.
-- Return the new FlatCurry program and a flag indicating whether
-- something has been changed.
transformFlatProg :: Options -> String -> Prog -> IO (Prog, Bool)
transformFlatProg opts modname
                  (Prog mname imports tdecls fdecls opdecls)= do
  lookupreqinfo <-
    if withAnalysis opts
    then do (mreqinfo,reqinfo) <- loadAnalysisWithImports reqValueAnalysis
                                                          modname imports
            printVerbose opts 2 $
              "\nResult of \"RequiredValue\" analysis:\n" ++
              showInfos (showAFType AText)
                        (if verbosity opts == 3 then reqinfo else mreqinfo)
            return (flip lookupProgInfo reqinfo)
    else return (flip lookup preludeBoolReqValues)
  let (stats,newfdecls) = unzip (map (transformFuncDecl opts lookupreqinfo)
                                     fdecls)
      numtranseqs = totalTransEqs stats
      numtranseqv = totalTransEqv stats
      numbeqs     = totalBEqs  stats
      csvfname    = mname ++ "_BOPTSTATS.csv"
  printVerbose opts 2 $ statSummary stats
  printVerbose opts 1 $
     "Total number of transformed (dis)equalities: " ++
     show numtranseqs ++ " (===) " ++
     (if eqvTrans opts then " and " ++ show numtranseqv ++ " (==)" else "") ++
     " (out of " ++ show numbeqs ++ ")"
  unless (verbosity opts < 2) $ do
    writeCSVFile csvfname (stats2csv stats)
    putStrLn ("Detailed statistics written to '" ++ csvfname ++"'")
  return ( Prog mname imports tdecls newfdecls opdecls
         , numtranseqs + numtranseqv > 0)

loadAnalysisWithImports :: (Read a, Show a, ReadWrite a, Eq a) => Analysis a
                        -> String -> [String] -> IO (ProgInfo a,ProgInfo a)
loadAnalysisWithImports analysis modname imports = do
  maininfo <- analyzeGeneric analysis modname >>= return . either id error
  impinfos <- mapM (\m -> analyzePublic analysis m >>=
                                                     return . either id error)
                    imports
  return $ (maininfo, foldr1 combineProgInfo (maininfo:impinfos))

showInfos :: (a -> String) -> ProgInfo a -> String
showInfos showi =
  unlines . map (\ (qn,i) -> snd qn ++ ": " ++ showi i)
          . (\p -> fst p ++ snd p) . progInfo2Lists

-- Transform a function declaration.
-- Some statistical information and the new function declaration are returned.
transformFuncDecl :: Options -> (QName -> Maybe AFType) -> FuncDecl
                  -> (TransStat, FuncDecl)
transformFuncDecl opts lookupreqinfo fdecl@(Func qf@(_,fn) ar vis texp rule) =
  if containsBeqRule opts rule
  then
    let (tst,trule) = transformRule opts lookupreqinfo (initTState qf) rule
    in ( TransStat fn beqs (numTransEqs tst) (numTransEqv tst)
       , Func qf ar vis texp trule )
  else (TransStat fn 0 0 0, fdecl)
 where
  beqs = numberBeqRule opts rule

-------------------------------------------------------------------------
-- State threaded through the program transformer:
-- * name of current function
-- * number of occurrences of (===) that are replaced by (=:=)
-- * number of occurrences of (==) that are replaced by (=:=)
data TState = TState { currFunc :: QName
                     , numTransEqs :: Int
                     , numTransEqv :: Int
                     }

initTState :: QName -> TState
initTState qf = TState qf 0 0

-- Increment number of transformed equalities.
incNumEqs :: TState -> TState
incNumEqs tst = tst { numTransEqs = numTransEqs tst + 1 }

-- Increment number of transformed equivalences.
incNumEqv :: TState -> TState
incNumEqv tst = tst { numTransEqv = numTransEqv tst + 1 }

-------------------------------------------------------------------------
--- Transform a FlatCurry program rule w.r.t. information about required
--- values. If there is an occurrence of `(e1===e2)` where the value `True`
--- is required, then this occurrence is replaced by
---
---     (Prelude.constrEq e1 e2)
---
--- Similarly, `(e1/==e2)` with required value `False` is replaced by
---
---     (not (Prelude.constrEq e1 e2))

transformRule :: Options -> (QName -> Maybe AFType) -> TState -> Rule
              -> (TState,Rule)
transformRule _ _ tst (External s) = (tst, External s)
transformRule opts lookupreqinfo tstr (Rule args rhs) =
  let (te,tste) = transformExp tstr rhs Any
   in (tste, Rule args te)
 where
  -- transform an expression w.r.t. a required value
  transformExp tst (Var i) _ = (Var i, tst)
  transformExp tst (Lit v) _ = (Lit v, tst)

  transformExp tst0 exp@(Comb ct qf es) reqval
    | reqval == aTrue && isBoolEqualCall opts True exp
    = case checkBoolEqualCall opts True (Comb ct qf tes) of
        Just (eqs,targs) -> ( Comb FuncCall (pre "constrEq") targs
                            , (if eqs then incNumEqs else incNumEqv) tst1 )
        Nothing          -> error "Internal error: Nothing in transfromExp"
    | reqval == aFalse && isBoolEqualCall opts False exp
    = case checkBoolEqualCall opts False (Comb ct qf tes) of
        Just (eqs,targs) -> ( Comb FuncCall (pre "not")
                                [Comb FuncCall (pre "constrEq") targs]
                            , (if eqs then incNumEqs else incNumEqv) tst1 )
        Nothing          -> error "Internal error: Nothing in transfromExp"
    | qf == pre "$" && length es == 2 &&
      (isFuncPartCall (head es) || isConsPartCall (head es))
    = transformExp tst0 (reduceDollar es) reqval
    | otherwise
    = (Comb ct qf tes, tst1)
   where
    reqargtypes = argumentTypesFor (lookupreqinfo qf) reqval
    (tes,tst1)  = transformExps tst0 (zip es reqargtypes)

  transformExp tst0 (Free vars e) reqval =
    let (te,tst1) = transformExp tst0 e reqval
     in (Free vars te, tst1)
  transformExp tst0 (Or e1 e2) reqval =
    let (te1,tst1) = transformExp tst0 e1 reqval
        (te2,tst2) = transformExp tst1 e2 reqval
     in (Or te1 te2, tst2)
  transformExp tst0 (Typed e t) reqval =
    let (te,tst1) = transformExp tst0 e reqval
     in (Typed te t, tst1)
  transformExp tst0 (Case ct e bs) reqval =
    let (te ,tst1) = transformExp tst0 e (caseArgType bs)
        (tbs,tst2) = transformBranches tst1 bs reqval
     in (Case ct te tbs, tst2)
  transformExp tst0 (Let bs e) reqval =
    let (tbes,tst1) = transformExps tst0 (zip (map snd bs) (repeat Any))
        (te,tst2) = transformExp tst1 e reqval
     in (Let (zip (map fst bs) tbes) te, tst2)

  transformExps tst [] = ([],tst)
  transformExps tst ((exp,rv):exps) =
    let (te, tste ) = transformExp tst exp rv
        (tes,tstes) = transformExps tste exps
     in (te:tes, tstes)

  transformBranches tst [] _ = ([],tst)
  transformBranches tst (br:brs) reqval =
    let (tbr,tst1) = transformBranch tst br reqval
        (tbrs,tst2) = transformBranches tst1 brs reqval
     in (tbr:tbrs, tst2)

  transformBranch tst (Branch pat be) reqval =
    let (tbe,tstb) = transformExp tst be reqval
     in (Branch pat tbe, tstb)

-------------------------------------------------------------------------
-- Check whether the expression argument is a call to a Boolean (dis)equality.
-- If this is the case, return a flag indicating whether it is a `(===)` call
-- and the actual arguments of the call.
-- Otherwise, return `Nothing`.
-- If the first argument is `True`, we check equalities ("===" or "=="),
-- otherwise we check disequalities  ("/=").
-- Since the equalities are defined in type classes,
-- a Boolean (dis)equality call can be
-- * an instance (dis)equality call: "_impl#==#Prelude.Eq#..." ... e1 e2
--   (where there can be additional arguments for other Eq dicts)
-- * a class (dis)equality call: apply (apply ("==" [dict]) e1) e2
--   (where dict is a dictionary parameter)
-- * a default instance (dis)equality call:
--   apply (apply ("_impl#==#Prelude.Eq#..." []) e1) e2
checkBoolEqualCall :: Options -> Bool -> Expr -> Maybe (Bool, [Expr])
checkBoolEqualCall opts eq exp = case exp of
  Comb FuncCall qf es ->
    if isEqNameOrInst qf && length es > 1
      then Just (isEqsNameOrInst qf,
                 -- drop possible Eq dictionary arguments:
                 drop (length es - 2) es)
      else if qf == pre "apply"
             then case es of
                    [Comb FuncCall qfa [Comb FuncCall qfe [_],e1],e2] ->
                      if qfa == pre "apply" && isEqNameOrInst qfe
                        then Just (isEqsNameOrInst qfe, [e1,e2])
                        else Nothing
                    [Comb FuncCall qfa [Comb FuncCall qfe [],e1],e2] ->
                      if qfa == pre "apply" && isEqNameOrInst qfe
                        then Just (isEqsNameOrInst qfe, [e1,e2])
                        else Nothing
                    _ -> Nothing
             else Nothing
  _ -> Nothing
 where
  isEqNameOrInst qf = isEqsNameOrInst qf || isEqvNameOrInst qf

  isEqsNameOrInst qf@(_,f) =
    if eq then qf == pre "===" || "_impl#===#Prelude.Data#" `isPrefixOf` f
          else qf == pre "/=="

  isEqvNameOrInst qf@(_,f) =
    eqvTrans opts && -- should we also transform (==)?
    if eq then qf == pre "==" || "_impl#==#Prelude.Eq#" `isPrefixOf` f
          else qf == pre "/=" || "_impl#/=#Prelude.Eq#" `isPrefixOf` f


-- Is this a call to a Boolean equality?
-- If the first argument is `True`, it must be an equality call,
-- otherwise an disequality call.
isBoolEqualCall :: Options -> Bool -> Expr -> Bool
isBoolEqualCall opts eq exp = isJust (checkBoolEqualCall opts eq exp)

-------------------------------------------------------------------------

--- Reduce an application of Prelude.$ to a combination:
reduceDollar :: [Expr] -> Expr
reduceDollar args = case args of
  [Comb (FuncPartCall n) qf es, arg2]
    -> Comb (if n==1 then FuncCall else (FuncPartCall (n-1))) qf (es++[arg2])
  [Comb (ConsPartCall n) qf es, arg2]
    -> Comb (if n==1 then ConsCall else (ConsPartCall (n-1))) qf (es++[arg2])
  _ -> error "reduceDollar"

--- Try to compute the required value of a case argument expression.
--- If one branch of the case expression is "False -> failed",
--- then the required value is `True` (this is due to the specific
--- translation of Boolean conditional rules of the front end).
--- If the case expression has one non-failing branch, the constructor
--- of this branch is chosen, otherwise it is `Any`.
caseArgType :: [BranchExpr] -> AType
caseArgType branches
  | not (null (tail branches)) &&
    branches!!1 == Branch (Pattern (pre "False") []) failedFC
  = aCons (pre "True")
  | length nfbranches /= 1
  = Any
  | otherwise = getPatCons (head nfbranches)
 where
  failedFC = Comb FuncCall (pre "failed") []

  nfbranches = filter (\ (Branch _ be) -> be /= failedFC) branches

  getPatCons (Branch (Pattern qc _) _) = aCons qc
  getPatCons (Branch (LPattern _)   _) = Any

--- Compute the argument types for a given abstract function type
--- and required value.
argumentTypesFor :: Maybe AFType -> AType -> [AType]
argumentTypesFor Nothing                _      = repeat Any
argumentTypesFor (Just EmptyFunc)       _      = repeat Any
argumentTypesFor (Just (AFType rtypes)) reqval =
  maybe (-- no exactly matching type, look for Any type:
         maybe (-- no Any type: if reqtype==Any, try lub of all other types:
                if (reqval==Any || reqval==AnyC) && not (null rtypes)
                then foldr1 lubArgs (map fst rtypes)
                else repeat Any)
               fst
               (find ((`elem` [AnyC,Any]) . snd) rtypes))
        fst
        (find ((==reqval) . snd) rtypes)
 where
  lubArgs xs ys = map (uncurry lubAType) (zip xs ys)


-- Does `Prelude.===` or `Prelude.==` occur in a rule?
containsBeqRule :: Options -> Rule -> Bool
containsBeqRule _    (External _) = False
containsBeqRule opts (Rule _ rhs) = containsBeqExp rhs
 where
  -- containsBeq an expression w.r.t. a required value
  containsBeqExp (Var _) = False
  containsBeqExp (Lit _) = False
  containsBeqExp exp@(Comb _ _ es) =
    isBoolEqualCall opts True exp || isBoolEqualCall opts False exp ||
    any containsBeqExp es
  containsBeqExp (Free _ e   ) = containsBeqExp e
  containsBeqExp (Or e1 e2   ) = containsBeqExp e1 || containsBeqExp e2
  containsBeqExp (Typed e _  ) = containsBeqExp e
  containsBeqExp (Case _ e bs) = containsBeqExp e || any containsBeqBranch bs
  containsBeqExp (Let bs e   ) = containsBeqExp e ||
                                 any containsBeqExp (map snd bs)

  containsBeqBranch (Branch _ be) = containsBeqExp be

-- Number of occurrences of `Prelude.===` or `Prelude./==` occurring in a rule:
numberBeqRule :: Options -> Rule -> Int
numberBeqRule _    (External _) = 0
numberBeqRule opts (Rule _ rhs) = numberBeqExp rhs
 where
  -- numberBeq an expression w.r.t. a required value
  numberBeqExp (Var _) = 0
  numberBeqExp (Lit _) = 0
  numberBeqExp exp@(Comb _ _ es) =
    case checkBoolEqualCall opts True exp of
      Just (_,targs) -> 1 + sum (map numberBeqExp targs)
      Nothing        -> case checkBoolEqualCall opts False exp of
                          Just (_,fargs) -> 1 + sum (map numberBeqExp fargs)
                          Nothing        -> sum (map numberBeqExp es)
  numberBeqExp (Free _ e) = numberBeqExp e
  numberBeqExp (Or e1 e2) = numberBeqExp e1 + numberBeqExp e2
  numberBeqExp (Typed e _) = numberBeqExp e
  numberBeqExp (Case _ e bs) = numberBeqExp e + sum (map numberBeqBranch bs)
  numberBeqExp (Let bs e) = numberBeqExp e + sum (map numberBeqExp (map snd bs))

  numberBeqBranch (Branch _ be) = numberBeqExp be

pre :: String -> QName
pre n = ("Prelude", n)

-------------------------------------------------------------------------
-- Loading prelude analysis result:
loadPreludeBoolReqValues :: IO [(QName, AFType)]
loadPreludeBoolReqValues = do
  maininfo <- analyzeInterface reqValueAnalysis "Prelude" >>=
                                                return . either id error
  return (filter (hasBoolReqValue . snd) maininfo)
 where
  hasBoolReqValue EmptyFunc = False
  hasBoolReqValue (AFType rtypes) =
    maybe False (const True) (find (isBoolReqValue . snd) rtypes)

  isBoolReqValue rt = rt == aFalse || rt == aTrue

-- Current relevant Boolean functions of the prelude:
preludeBoolReqValues :: [(QName, AFType)]
preludeBoolReqValues =
 [(pre "&&",    AFType [([Any,Any],aFalse), ([aTrue,aTrue],aTrue)])
 ,(pre "not",   AFType [([aTrue],aFalse), ([aFalse],aTrue)])
 ,(pre "||",    AFType [([aFalse,aFalse],aFalse), ([Any,Any],aTrue)])
 ,(pre "&",     AFType [([aTrue,aTrue],aTrue)])
 ,(pre "solve", AFType [([aTrue],aTrue)])
 ,(pre "&&>",   AFType [([aTrue,Any],AnyC)])
 ]

--- Map a constructor into an abstract value representing this constructor:
aCons :: QName -> AType
aCons qn = Cons [qn]

--- Abstract `False` value
aFalse :: AType
aFalse = aCons (pre "False")

--- Abstract `True` value
aTrue :: AType
aTrue  = aCons (pre "True")

-------------------------------------------------------------------------
--- Statistical information (e.g., for benchmarking the tool):
--- * function name
--- * number of Boolean (dis)equalities/equivalences in the rule
--- * number of transformed (dis)equalities in the rule
--- * number of transformed (dis)equivalences in the rule
data TransStat = TransStat String Int Int Int

--- Number of all (===) transformations:
totalTransEqs :: [TransStat] -> Int
totalTransEqs = sum . map (\ (TransStat _ _ teqs _) -> teqs)

--- Number of all (==) transformations:
totalTransEqv :: [TransStat] -> Int
totalTransEqv = sum . map (\ (TransStat _ _ _ teqv) -> teqv)

--- Number of all Boolean (dis)equalities:
totalBEqs :: [TransStat] -> Int
totalBEqs = sum . map (\ (TransStat _ beqs _ _) -> beqs)

--- Show a summary of the actual transformations:
statSummary :: [TransStat] -> String
statSummary = concatMap showSum
 where
  showSum (TransStat fn _ teqs teqv) =
    if teqs + teqv == 0
      then ""
      else (if teqs > 0
              then showFun fn ++ showNOccs teqs ++
                   " of (===) transformed into (=:=)\n"
              else "") ++
           (if teqv > 0
              then showFun fn ++ showNOccs teqv ++
                   " of (==) transformed into (=:=)\n"
              else "")

  showFun fn  = "Function " ++ fn ++ ": "
  showNOccs n = if n==1 then "one occurrence" else show n ++ " occurrences"

--- Translate statistics into CSV format:
stats2csv :: [TransStat] -> [[String]]
stats2csv stats =
  ["Function","Boolean equalities",
   "Transformed equalities", "Transformed equivalences"] :
  map (\ (TransStat fn beqs teqs teqv) -> fn : map show [beqs, teqs, teqv])
      stats

-------------------------------------------------------------------------
