-----------------------------------------------------------------------------
--- Translator for Curry programs to implement default rules
--- and deterministic functions.
---
--- @author Michael Hanus
--- @version October 2016
-----------------------------------------------------------------------------

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty
import Char(isDigit,digitToInt)
import DefaultRuleUsage
import Directory
import Distribution
import FilePath (takeDirectory)
import List(isPrefixOf,isSuffixOf,partition)
import System
import TheoremUsage

--------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText =
     "Transformation Tool for Curry with Default Rules (Version of 08/06/16)"
   bannerLine = take (length bannerText) (repeat '=')

------------------------------------------------------------------------

-- Available translation schemes
data TransScheme = SpecScheme -- as specified in the PADL'16 paper
                 | NoDupScheme -- scheme without checking conditions twice
 deriving (Eq,Show)

-- The default translation scheme:
defaultTransScheme :: TransScheme
defaultTransScheme = if curryCompiler == "kics2"
                     then SpecScheme -- due to bug in KiCS2
                     else SpecScheme -- NoDupScheme

------------------------------------------------------------------------
-- Start default rules transformation in "preprocessor mode".
-- The Curry program must be read with readUntypedCurry in order to
-- process DET annotations!
transDefaultRules :: Int -> [String] -> String -> CurryProg
                  -> IO (Maybe CurryProg)
transDefaultRules verb moreopts _ prog = do
  when (verb>1) $ putStr banner
  trscm <- processOpts moreopts
  when (verb>1) $ putStrLn ("Translation scheme: " ++ show trscm)
  mbtransprog <- translateProg trscm prog
  maybe (return Nothing)
   (\ (detfuncnames,newprog) -> do
      -- check whether we have files with determinism proofs:
      prfiles <- getProofFiles (takeDirectory (modNameToPath (progName prog)))
      detfuncnamesWOproofs <- filterProofObligation verb prfiles detfuncnames
      when (verb>0) $ printProofObligation detfuncnamesWOproofs
      return (Just newprog))
   mbtransprog
 where
  processOpts opts = case opts of
    []       -> return defaultTransScheme
    [scheme] ->
       if scheme == "nodupscheme"
       then if curryCompiler == "kics2"
            then return SpecScheme -- due to bug in KiCS2!!!
            else return NoDupScheme
       else if scheme == "specscheme"
            then return SpecScheme
            else showError
    _ -> showError
   where
    showError = do
      putStrLn $ "Unknown options (ignored): " ++ unwords opts
      return defaultTransScheme

-- Filter proof obligations for determinism annotation w.r.t. to existence
-- of proof files:
filterProofObligation :: Int -> [String] -> [QName] -> IO [QName]
filterProofObligation _ _ [] = return []
filterProofObligation verb prooffiles (qf@(_,fn) : qfs) = do
  let hasdetproof = existsProofFor (determinismTheoremFor fn) prooffiles
  when (hasdetproof && verb>0) $ putStrLn $
    "Proofs for determinism property of " ++ showQName qf ++ " found:\n" ++
    unlines (filter (isProofFileNameFor (determinismTheoremFor fn)) prooffiles)
  filterqfs <- filterProofObligation verb prooffiles qfs
  return (if hasdetproof then filterqfs else qf : filterqfs)


printProofObligation :: [QName] -> IO ()
printProofObligation qfs = unless (null qfs) $ do
  putStrLn line
  putStrLn "PROOF OBLIGATIONS:"
  mapIO_ (\qn -> putStrLn $ showQName qn ++" is a deterministic operation.") qfs
  putStrLn line
 where
  line = take 70 (repeat '=')

showQName :: QName -> String
showQName (qn,fn) = "'" ++ qn ++ "." ++ fn ++ "'"

------------------------------------------------------------------------
-- Main transformation: transform a Curry program with default rules
-- and deterministic functions into a new Curry program where these
-- features are implemented by standard Curry features.
-- Moreover, the list of deterministic functions is returned
-- (to show the proof obligations to ensure completeness of the
-- transformation).
-- If the program was not transformed, `Nothing` is returned.

translateProg :: TransScheme -> CurryProg -> IO (Maybe ([QName],CurryProg))
translateProg trscm
  prog@(CurryProg mn imps dfltdecl clsdecls instdecls tdecls fdecls ops) = do
  let usageerrors = checkDefaultRules prog
  unless (null usageerrors) $ do
    putStr (unlines $ "ERROR: ILLEGAL USE OF DEFAULT RULES:" :
               map (\ ((_,fn),err) -> fn ++ " (module " ++ mn ++ "): " ++ err)
                   usageerrors)
    error "Transformation aborted"
  -- now we do not have to check the correct usage of default rules...
  return $ if null deffuncs && null detfuncnames
         then Nothing
         else Just (detfuncnames, CurryProg mn newimports dfltdecl clsdecls
                                            instdecls tdecls newfdecls ops)
 where
  newimports       = if setFunMod `elem` imps then imps else setFunMod:imps
  detfuncnames     = map funcName (filter isDetFun fdecls)
  undetfuncs       = concatMap (transDetFun detfuncnames) fdecls
  (deffuncs,funcs) = partition isDefaultFunc undetfuncs
  defrules         = map func2rule deffuncs
  newfdecls        = concatMap (transFDecl trscm defrules) funcs

------------------------------------------------------------------------
-- implementation of deterministic function transformation:

-- Is the function declaration marked as a deterministic function?
isDetFun :: CFuncDecl -> Bool
isDetFun (CmtFunc _ qf ar vis texp rules) =
  isDetFun (CFunc qf ar vis texp rules)
isDetFun (CFunc _ _ _ (CQualType _ texp) _) = hasDetResultType texp
  where
   hasDetResultType (CTVar _)        = False
   hasDetResultType (CTCons _)       = False
   hasDetResultType (CFuncType _ rt) = hasDetResultType rt
   hasDetResultType (CTApply tc _)   = tc == CTCons (pre "DET")

-- translate a function (where the names of all deterministic functions
-- is provided as a first argument):
transDetFun :: [QName] -> CFuncDecl -> [CFuncDecl]
transDetFun detfnames (CmtFunc _ qf ar vis texp rules) =
  transDetFun detfnames (CFunc qf ar vis texp rules)
transDetFun detfnames fdecl@(CFunc qf@(mn,fn) ar vis texp rules)
 | qf `elem` detfnames
 = [CFunc qf ar vis (removeDetResultType texp) [newdetrule],
    CFunc neworgname ar Private (removeDetResultType texp) rules]
 | isDefaultFunc fdecl && (mn, fromDefaultName fn) `elem` detfnames
  -- rename default rule of a deterministic function:
 = [CFunc (mn, fromDefaultName fn ++ orgsuffix ++ "'default") ar vis texp rules]
 | otherwise = [fdecl]
 where
  -- new name for original function (TODO: check for unused name)
  neworgname = (mn,fn++orgsuffix)
  orgsuffix = "_ORGNDFUN"

  newdetrule =
    CRule (map CPVar argvars)
          (CSimpleRhs (applyF (setFunMod, "selectValue")
                              [applyF (setFunMod, "set"++show ar)
                                      (CSymbol neworgname : map CVar argvars)])
                      [])

  argvars = map (\i->(i,"x"++show i)) [1..ar]

removeDetResultType :: CQualTypeExpr -> CQualTypeExpr
removeDetResultType (CQualType clsctxt te) = CQualType clsctxt (removeDet te)
 where
  removeDet tv@(CTVar _)      = tv
  removeDet tc@(CTCons _)     = tc
  removeDet (CFuncType t1 t2) = CFuncType t1 (removeDet t2)
  removeDet t@(CTApply tc ta) = if tc == CTCons (pre "DET") then ta else t

------------------------------------------------------------------------
-- implementation of default rule transformation:

-- Extract the arity and default rule for a default function definition:
func2rule :: CFuncDecl -> (QName,(Int,CRule))
func2rule (CFunc (mn,fn) ar _ _ rules) =
  ((mn, fromDefaultName fn), (ar, head rules))
func2rule (CmtFunc _ qf ar vis texp rules) =
  func2rule (CFunc qf ar vis texp rules)

-- Translates a function declaration into a new one that respects
-- the potential default rule (the second argument contains
-- the list of all default rules).
transFDecl :: TransScheme -> [(QName,(Int,CRule))] -> CFuncDecl -> [CFuncDecl]
transFDecl trscm defrules (CmtFunc _ qf ar vis texp rules) =
  transFDecl trscm defrules (CFunc qf ar vis texp rules)
transFDecl trscm defrules fdecl@(CFunc qf@(mn,fn) ar vis texp rules) =
  maybe [fdecl]
        (\ (_,defrule) ->
             if trscm == SpecScheme
             then [CFunc neworgname ar Private texp rules,
                   transFDecl2ApplyCond applyname fdecl,
                   CFunc deffunname ar Private texp
                         [transDefaultRule applyname ar defrule],
                   CFunc qf ar vis texp [neworgrule_SpecScheme]]
             else -- trscm == NoDupScheme
                  [transFDecl2FunRHS applyname fdecl,
                   CFunc deffunname ar Private texp [defrule],
                   CFunc qf ar vis texp [neworgrule_NoDupScheme]]
        )
        (lookup qf defrules)
 where
  -- new names for auxiliary functions (TODO: check for unused name)
  neworgname = (mn,fn++"_ORGRULES")
  applyname  = (mn,fn++"_APPLICABLE")
  deffunname = (mn,fn++"_DEFAULT")

  neworgrule_SpecScheme =
    CRule (map CPVar argvars)
          (CSimpleRhs (applyF (pre "?")
                              [applyF neworgname (map CVar argvars),
                               applyF deffunname (map CVar argvars)])
                      [])

  neworgrule_NoDupScheme =
    CRule (map CPVar argvars)
          (CSimpleRhs
             (CLetDecl [CLocalPat (CPVar (0,"x0"))
                         (CSimpleRhs
                            (applyF (setFunMod,"set"++show ar)
                                    (CSymbol applyname : map CVar argvars))
                            [])]
                       (applyF (pre "if_then_else")
                          [applyF (setFunMod,"isEmpty") [CVar (0,"x0")],
                           applyF deffunname (map CVar argvars),
                           applyF (setFunMod,"chooseValue")
                                  [CVar (0,"x0"), preUnit]]))
             [])

  argvars = map (\i->(i,"x"++show i)) [1..ar]

-- Translates a function declaration into one where the right-hand side
-- is always Prelude.(), i.e., it just checks for applicability.
-- The first argument is the new name of the translated function.
transFDecl2ApplyCond :: QName -> CFuncDecl -> CFuncDecl
transFDecl2ApplyCond nqf (CmtFunc _ qf ar vis texp rules) =
  transFDecl2ApplyCond nqf (CFunc qf ar vis texp rules)
transFDecl2ApplyCond nqf (CFunc _ ar _ texp rules) =
  CFunc nqf ar Private (adjustResultTypeToUnit texp) (map rule2cond rules)
 where
  rule2cond (CRule rpats (CSimpleRhs _ rlocals)) =
    let singlepatvars = extractSingles (concatMap varsOfPat rpats ++
                                        concatMap varsOfLDecl rlocals)
     in CRule (map (anonymPat singlepatvars) rpats)
              (CSimpleRhs preUnit rlocals)
  rule2cond (CRule rpats (CGuardedRhs gds rlocals)) =
    let singlepatvars = extractSingles (concatMap varsOfPat rpats ++
                                        concatMap (varsOfExp . fst) gds ++
                                        concatMap varsOfLDecl rlocals)
     in CRule (map (anonymPat singlepatvars) rpats)
              (CGuardedRhs (map (\gd -> (fst gd,preUnit)) gds) rlocals)

-- Adjust the result type of a function type by setting this type to ():
adjustResultTypeToUnit :: CQualTypeExpr -> CQualTypeExpr
adjustResultTypeToUnit (CQualType clsctxt te) =
  CQualType clsctxt (adjustRType te)
 where
  adjustRType texp =
    if texp == preUntyped
      then texp
      else case texp of
             CFuncType te1 te2 -> CFuncType te1 (adjustRType te2)
             _                 -> unitType

-- Translates a function declaration into one where the right-hand side
-- is encapsulated in a unary function, i.e., it just checks for applicability
-- and can later be applied to evaluate its right-hand side.
-- The first argument is the new name of the translated function.
transFDecl2FunRHS :: QName -> CFuncDecl -> CFuncDecl
transFDecl2FunRHS nqf (CmtFunc _ qf ar vis texp rules) =
  transFDecl2FunRHS nqf (CFunc qf ar vis texp rules)
transFDecl2FunRHS nqf (CFunc _ ar _ texp rules) =
  CFunc nqf ar Private (adjustResultTypeToFunRHS texp) (map rule2funrhs rules)
 where
  rule2funrhs (CRule rpats (CSimpleRhs rhsexp rlocals)) =
     CRule rpats
           (CSimpleRhs (CLambda [CPVar (999,"_")] rhsexp) rlocals)
  rule2funrhs (CRule rpats (CGuardedRhs gds rlocals)) =
    CRule rpats
          (CGuardedRhs
             (map (\ (gd,rhs) -> (gd,(CLambda [CPVar (999,"_")] rhs))) gds)
             rlocals)

-- Adjust the result type of a function type by setting the result type
-- `te` to `() -> texp`:
adjustResultTypeToFunRHS :: CQualTypeExpr -> CQualTypeExpr
adjustResultTypeToFunRHS (CQualType clsctxt te) =
  CQualType clsctxt (adjustRType te)
 where
  adjustRType texp =
    if texp == preUntyped
      then texp
      else case texp of
             CFuncType te1 te2 -> CFuncType te1 (adjustRType te2)
             _                 -> CFuncType unitType texp

transDefaultRule :: QName -> Int -> CRule -> CRule
transDefaultRule _ _ (CRule _ (CGuardedRhs _ _)) =
  error "Cannot yet transform guarded default rules!"
transDefaultRule condfunname ar (CRule pats (CSimpleRhs exp locals)) =
  CRule newpats (CGuardedRhs [(checkCond,exp)] locals)
 where
  checkCond = applyF (setFunMod,"isEmpty")
                     [applyF (setFunMod,"set"++show ar)
                             (CSymbol condfunname : args)]

  (newpats,args) = unzip (map arg2patexp (zip [1001..] pats))
  
  arg2patexp (i,pat) = case pat of
    CPVar v     -> if snd v=="_"
                     then let newvar = (i,"patvar_"++show i)
                           in (CPVar newvar, CVar newvar)
                     else (pat, CVar v)
    CPAs asv _  -> (pat, CVar asv)
    _           -> let newvar = (i,"patvar_"++show i)
                    in (CPAs newvar pat, CVar newvar)
  
------------------------------------------------------------------------

preUnit :: CExpr
preUnit = CSymbol (pre "()")

preUntyped :: CTypeExpr
preUntyped = CTCons (pre "untyped")

setFunMod :: String
setFunMod = "SetFunctions"

--- Extracts all elements with a single occurrence in a given list.
extractSingles :: Eq a => [a] -> [a]
extractSingles [] = []
extractSingles (x:xs) =
  if null (filter (==x) xs)
  then x : extractSingles xs
  else extractSingles (filter (/=x) xs)

--- Replaces all variables occurring in the first argument by
--- anonymous variables in a pattern.
anonymPat :: [(Int,String)] -> CPattern -> CPattern
anonymPat vs (CPVar v) = CPVar (if v `elem` vs then (fst v,"_") else v)
anonymPat _  (CPLit l) = CPLit l
anonymPat vs (CPComb qc pats) = CPComb qc (map (anonymPat vs) pats)
anonymPat vs (CPAs v pat) =
  if v `elem` vs then anonymPat vs pat
                 else CPAs v (anonymPat vs pat)
anonymPat vs (CPFuncComb qf pats) = CPFuncComb qf (map (anonymPat vs) pats)
anonymPat vs (CPLazy pat) = CPLazy (anonymPat vs pat)
anonymPat vs (CPRecord qc recpats) =
  CPRecord qc (map (\ (n,p) -> (n, anonymPat vs p)) recpats)

------------------------------------------------------------------------
