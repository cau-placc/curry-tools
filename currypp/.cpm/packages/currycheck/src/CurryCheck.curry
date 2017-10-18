-------------------------------------------------------------------------
--- This is the implementation of the currycheck tool.
--- It performs various checks on Curry programs:
---
--- * Correct usage of set functions, non-strict unification,
---   default rules, DET annotations, contracts
--- * All EasyCheck tests are extracted and checked
--- * For all functions declared as deterministic,
---   determinism properties are generated and checked.
--- * For functions with postconditions (f'post), checks for postconditions
---   are generated (together with possible preconditions)
--- * For functions with specification (f'spec), checks for satisfaction
---   of these specifications are generated
---   (together with possible preconditions).
---
--- @author Michael Hanus, Jan-Patrick Baye
--- @version February 2017
-------------------------------------------------------------------------

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty    (showCProg)
import AbstractCurry.Transform (renameCurryModule, trCTypeExpr
                               ,updCProg, updQNamesInCProg)
import AnsiCodes
import Distribution
import FilePath                ((</>), pathSeparator, takeDirectory)
import qualified FlatCurry.Types as FC
import FlatCurry.Files
import qualified FlatCurry.Goodies as FCG
import GetOpt
import IO
import List
import Maybe                   (fromJust, isJust)
import ReadNumeric             (readNat)
import System                  (system, exitWith, getArgs, getPID, getEnviron)

import CheckDetUsage           (checkDetUse, containsDetOperations)
import ContractUsage
import CurryCheckConfig        (packagePath, packageVersion)
import DefaultRuleUsage        (checkDefaultRules, containsDefaultRules)
import PropertyUsage
import SimplifyPostConds       (simplifyPostConditionsWithTheorems)
import TheoremUsage
import UsageCheck              (checkBlacklistUse, checkSetUse)

--- Maximal arity of check functions and tuples currently supported:
maxArity :: Int
maxArity = 5

-- Banner of this tool:
ccBanner :: String
ccBanner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "CurryCheck: a tool for testing Curry programs (Version " ++
                packageVersion ++ " of 01/06/2017)"
   bannerLine = take (length bannerText) (repeat '-')

-- Help text
usageText :: String
usageText = usageInfo ("Usage: curry check [options] <module names>\n") options

-------------------------------------------------------------------------
-- Representation of command line options.
data Options = Options
  { optHelp     :: Bool
  , optVerb     :: Int
  , optKeep     :: Bool
  , optMaxTest  :: Int
  , optMaxFail  :: Int
  , optDefType  :: String
  , optSource   :: Bool
  , optProp     :: Bool
  , optSpec     :: Bool
  , optDet      :: Bool
  , optProof    :: Bool
  , optColor    :: Bool
  , optMainProg :: String
  }

-- Default command line options.
defaultOptions :: Options
defaultOptions  = Options
  { optHelp     = False
  , optVerb     = 1
  , optKeep     = False
  , optMaxTest  = 0
  , optMaxFail  = 0
  , optDefType  = "Ordering"
  , optSource   = True
  , optProp     = True
  , optSpec     = True
  , optDet      = True
  , optProof    = True
  , optColor    = True
  , optMainProg = ""
  }

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"] (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"] (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
            (OptArg (maybe (checkVerb 3) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show test names (default)\n2: show more information about test generation\n3: show test data (same as `-v')\n4: show also some debug information"
  , Option "k" ["keep"] (NoArg (\opts -> opts { optKeep = True }))
           "keep temporarily generated program files"
  , Option "m" ["maxtests"]
           (ReqArg (safeReadNat (\n opts -> opts { optMaxTest = n })) "<n>")
           "maximal number of tests (default: 100)"
  , Option "f" ["maxfails"]
           (ReqArg (safeReadNat (\n opts -> opts { optMaxFail = n })) "<n>")
           "maximal number of condition failures\n(default: 10000)"
  , Option "d" ["deftype"]
           (ReqArg checkDefType "<t>")
           "type for defaulting polymorphic tests:\nBool | Int | Char | Ordering (default)"
  , Option "" ["nosource"]
           (NoArg (\opts -> opts { optSource = False }))
           "do not perform source code checks"
  , Option "" ["noprop"]
           (NoArg (\opts -> opts { optProp = False }))
           "do not perform any property tests"
  , Option "" ["nospec"]
           (NoArg (\opts -> opts { optSpec = False }))
           "do not perform specification/postcondition tests"
  , Option "" ["nodet"]
           (NoArg (\opts -> opts { optDet = False }))
           "do not perform determinism tests"
  , Option "" ["noproof"]
           (NoArg (\opts -> opts { optProof = False }))
           "do not consider proofs to simplify properties"
  , Option "" ["nocolor"]
           (NoArg (\opts -> opts { optColor = False }))
           "do not use colors when showing tests"
  , Option "" ["mainprog"]
           (ReqArg (\s opts -> opts { optMainProg = s }) "<prog>")
           "name of generated main program\n(default: TEST<pid>.curry)"
  ]
 where
  safeReadNat opttrans s opts =
   let numError = error "Illegal number argument (try `-h' for help)" in
    maybe numError
          (\ (n,rs) -> if null rs then opttrans n opts else numError)
          (readNat s)

  checkVerb n opts = if n>=0 && n<5
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

  checkDefType s opts = if s `elem` ["Bool","Int","Char","Ordering"]
                        then opts { optDefType = s }
                        else error "Illegal default type (try `-h' for help)"

--- Further option processing, e.g., setting coloring mode.
processOpts :: Options -> IO Options
processOpts opts = do
  isterm <- hIsTerminalDevice stdout
  return $ if isterm then opts else opts { optColor = False}

isQuiet :: Options -> Bool
isQuiet opts = optVerb opts == 0

--- Print second argument if verbosity level is not quiet:
putStrIfNormal :: Options -> String -> IO ()
putStrIfNormal opts s = unless (isQuiet opts) (putStr s >> hFlush stdout)

--- Print second argument if verbosity level > 1:
putStrIfVerbose :: Options -> String -> IO ()
putStrIfVerbose opts s = when (optVerb opts > 1) (putStr s >> hFlush stdout)

--- Print second argument if verbosity level > 3:
putStrLnIfDebug :: Options -> String -> IO ()
putStrLnIfDebug opts s = when (optVerb opts > 3) (putStrLn s >> hFlush stdout)

--- use some coloring (from library AnsiCodes) if color option is on:
withColor :: Options -> (String -> String) -> String -> String
withColor opts coloring = if optColor opts then coloring else id

-------------------------------------------------------------------------
-- The names of suffixes added to specific tests.

defTypeSuffix :: String
defTypeSuffix = "_ON_BASETYPE"

postCondSuffix :: String
postCondSuffix = "SatisfiesPostCondition"

satSpecSuffix :: String
satSpecSuffix = "SatisfiesSpecification"

isDetSuffix :: String
isDetSuffix = "IsDeterministic"

-------------------------------------------------------------------------
-- Internal representation of tests extracted from a Curry module.
-- A test is either a property test (with a name, type, source line number)
-- or an IO test (with a name and source line number).
data Test = PropTest QName CTypeExpr Int | IOTest QName Int

-- Is the test an IO test?
isIOTest :: Test -> Bool
isIOTest t = case t of IOTest _ _ -> True
                       _          -> False
-- Is the test a unit test?
isUnitTest :: Test -> Bool
isUnitTest t = case t of PropTest _ texp _ -> null (argTypes texp)
                         _                 -> False

-- Is the test a property test?
isPropTest :: Test -> Bool
isPropTest t = case t of PropTest _ texp _ -> not (null (argTypes texp))
                         _                 -> False

-- The name of a test:
getTestName :: Test -> QName
getTestName (PropTest n _ _) = n
getTestName (IOTest   n   _) = n

-- The line number of a test:
getTestLine :: Test -> Int
getTestLine (PropTest _ _ n) = n
getTestLine (IOTest   _   n) = n

-- Generates a useful error message for tests (with module and line number)
genTestMsg :: String -> Test -> String
genTestMsg file test =
  snd (getTestName test) ++
  " (module " ++ file ++ ", line " ++ show (getTestLine test) ++ ")"

-------------------------------------------------------------------------
-- Representation of the information about a module to be tested:
-- * the original name of the module to be tested
-- * the name of the transformed (public) test module
-- * static errors (e.g., illegal uses of set functions)
-- * test operations
-- * name of generators defined in this module (i.e., starting with "gen"
--   and of appropriate result type)
data TestModule = TestModule
  { orgModuleName  :: String
  , testModuleName :: String
  , staticErrors   :: [String]
  , propTests      :: [Test]
  , generators     :: [QName]
  }

-- A test module with only static errors.
staticErrorTestMod :: String -> [String] -> TestModule
staticErrorTestMod modname staterrs =
 TestModule modname modname staterrs [] []

-- Is this a test module that should be tested?
testThisModule :: TestModule -> Bool
testThisModule tm = null (staticErrors tm) && not (null (propTests tm))

-- Extracts all user data types used as test data generators.
userTestDataOfModule :: TestModule -> [QName]
userTestDataOfModule testmod = concatMap testDataOf (propTests testmod)
 where
  testDataOf (IOTest _ _) = []
  testDataOf (PropTest _ texp _) = unionOn userTypesOf (argTypes texp)

  userTypesOf (CTVar _) = []
  userTypesOf (CFuncType from to) = union (userTypesOf from) (userTypesOf to)
  userTypesOf (CTCons (mn,tc)) = if mn == preludeName then [] else [(mn,tc)]
  userTypesOf (CTApply tc ta) = union (userTypesOf tc) (userTypesOf ta)

  unionOn f = foldr union [] . map f

-------------------------------------------------------------------------
-- Transform all tests of a module into an appropriate call of EasyCheck:
createTests :: Options -> String -> TestModule -> [CFuncDecl]
createTests opts mainmodname tm = map createTest (propTests tm)
 where
  createTest test =
    cfunc (mainmodname, (genTestName $ getTestName test)) 0 Public
          (emptyClassType (ioType (maybeType stringType)))
          (case test of PropTest   name t _ -> propBody name (argTypes t) test
                        IOTest name       _ -> ioTestBody name test)

  msgOf test = string2ac $ genTestMsg (orgModuleName tm) test

  testmname = testModuleName tm

  genTestName (modName, fName) = fName ++ "_" ++ modNameToId modName

  easyCheckFuncName arity =
    if arity>maxArity
    then error $ "Properties with more than " ++ show maxArity ++
                 " parameters are currently not supported!"
    else (easyCheckExecModule,"checkWithValues" ++ show arity)

  propBody (_, name) argtypes test =
    [simpleRule [] $
       CLetDecl [CLocalPat (CPVar msgvar) (CSimpleRhs (msgOf test) [])]
                (applyF (easyCheckExecModule, "checkPropWithMsg")
                  [CVar msgvar
                  ,applyF (easyCheckFuncName (length argtypes)) $
                    [configOpWithMaxFail, CVar msgvar] ++
                    (map (\t ->
                          applyF (easyCheckModule,"valuesOfSearchTree")
                            [if isPAKCS || useUserDefinedGen t || isFloatType t
                             then type2genop mainmodname tm t
                             else applyF (searchTreeModule,"someSearchTree")
                                         [constF (pre "unknown")]])
                         argtypes) ++
                    [CSymbol (testmname,name)]
                  ])]
   where
    useUserDefinedGen texp = case texp of
      CTVar _       -> error "No polymorphic generator!"
      CFuncType _ _ -> error "No generator for functional types!"
      CTApply _ _   ->
        maybe (error "No generator for type applications!")
              (\ ((_,tc),_) -> isJust
                         (find (\qn -> "gen"++tc == snd qn) (generators tm)))
              (tconsArgsOfType texp)
      CTCons (_,tc) -> isJust
                         (find (\qn -> "gen"++tc == snd qn) (generators tm))

    configOpWithMaxTest =
      let n = optMaxTest opts
       in if n==0 then stdConfigOp
                  else applyF (easyCheckExecModule,"setMaxTest")
                              [cInt n, stdConfigOp]

    configOpWithMaxFail =
      let n = optMaxFail opts
       in if n==0 then configOpWithMaxTest
                  else applyF (easyCheckExecModule,"setMaxFail")
                              [cInt n, configOpWithMaxTest]

    msgvar = (0,"msg")

  stdConfigOp = constF (easyCheckConfig opts)

  ioTestBody (_, name) test =
    [simpleRule [] $ applyF (easyCheckExecModule,"checkPropIOWithMsg")
                            [stdConfigOp, msgOf test, CSymbol (testmname,name)]]

-- The configuration option for EasyCheck
easyCheckConfig :: Options -> QName
easyCheckConfig opts =
  (easyCheckExecModule,
   if isQuiet opts     then "quietConfig"   else
   if optVerb opts > 2 then "verboseConfig"
                       else "easyConfig")

-- Translates a type expression into calls to generator operations.
type2genop :: String -> TestModule -> CTypeExpr -> CExpr
type2genop _ _ (CTVar _)       = error "No polymorphic generator!"
type2genop _ _ (CFuncType _ _) = error "No generator for functional types!"
type2genop mainmod tm (CTCons qt) =
  constF (typename2genopname mainmod (generators tm) qt)
type2genop mainmod tm te@(CTApply _ _) =
  maybe (error "No generator for type applications!")
        (\ (qt,targs) -> applyF (typename2genopname mainmod (generators tm) qt)
                                (map (type2genop mainmod tm) targs))
        (tconsArgsOfType te)

isFloatType :: CTypeExpr -> Bool
isFloatType texp = case texp of CTCons tc -> tc == (preludeName,"Float")
                                _         -> False

typename2genopname :: String -> [QName] -> QName -> QName
typename2genopname mainmod definedgenops (mn,tc)
  | isJust maybeuserdefined -- take user-defined generator:
  = fromJust maybeuserdefined
  | mn==preludeName &&
    tc `elem` ["Bool","Int","Float","Char","Maybe","Either","Ordering"]
  = (generatorModule, "gen" ++ tc)
  | mn==preludeName && tc `elem` ["[]","()","(,)","(,,)","(,,,)","(,,,,)"]
  = (generatorModule, "gen" ++ transTC tc)
  | otherwise -- we use our own generator:
  = (mainmod, "gen_" ++ modNameToId mn ++ "_" ++ tc)
 where
  maybeuserdefined = find (\qn -> "gen"++tc == snd qn) definedgenops

  transTC tcons | tcons == "[]"     = "List"
                | tcons == "()"     = "Unit"
                | tcons == "(,)"    = "Pair"
                | tcons == "(,,)"   = "Triple"
                | tcons == "(,,,)"  = "Tuple4"
                | tcons == "(,,,,)" = "Tuple5"

-------------------------------------------------------------------------
-- Turn all functions into public ones.
-- This ensures that all tests can be executed.
makeAllPublic :: CurryProg -> CurryProg
makeAllPublic (CurryProg modname imports dfltdecl clsdecls instdecls
                         typedecls functions opdecls) =
  CurryProg modname stimports dfltdecl clsdecls instdecls
            typedecls publicFunctions opdecls
 where
  stimports = if generatorModule `elem` imports &&
                 searchTreeModule `notElem` imports
              then searchTreeModule : imports -- just to be safe if module
                                              -- contains generator definitions
              else imports

  publicFunctions = map makePublic $ map ignoreComment functions

  -- since we create a copy of the module, we can ignore unnecessary data
  ignoreComment :: CFuncDecl -> CFuncDecl
  ignoreComment (CmtFunc _ name arity visibility typeExpr rules) =
    CFunc name arity visibility typeExpr rules
  ignoreComment x@(CFunc      _     _          _        _     _) = x

  makePublic :: CFuncDecl -> CFuncDecl
  makePublic (CFunc name arity _      typeExpr rules) =
              CFunc name arity Public typeExpr rules
  makePublic (CmtFunc cmt name arity _      typeExpr rules) =
              CmtFunc cmt name arity Public typeExpr rules

-- classify the tests as either PropTest or IOTest
classifyTests :: [CFuncDecl] -> [Test]
classifyTests = map makeProperty
 where
  makeProperty test = if isPropIOType ftype
                      then IOTest (funcName test) 0
                      else PropTest (funcName test) ftype 0
   where ftype = typeOfQualType (funcType test)

-- Extracts all tests from a given Curry module and transforms
-- all polymorphic tests into tests on a base type.
-- The result contains a triple consisting of all actual tests,
-- all ignored tests, and the public version of the original module.
transformTests :: Options -> String -> CurryProg
               -> IO ([CFuncDecl],[CFuncDecl],CurryProg)
transformTests opts srcdir
               prog@(CurryProg mname imps dfltdecl clsdecls instdecls
                               typeDecls functions opDecls) = do
  theofuncs <- if optProof opts then getTheoremFunctions srcdir prog
                                else return []
  simpfuncs <- simplifyPostConditionsWithTheorems (optVerb opts) theofuncs funcs
  let preCondOps  = preCondOperations simpfuncs
      postCondOps = map ((\ (mn,fn) -> (mn, fromPostCondName fn)) . funcName)
                        (funDeclsWith isPostCondName simpfuncs)
      specOps     = map ((\ (mn,fn) -> (mn, fromSpecName fn)) . funcName)
                        (funDeclsWith isSpecName simpfuncs)
      -- generate post condition tests:
      postCondTests = concatMap (genPostCondTest preCondOps postCondOps) funcs
      -- generate specification tests:
      specOpTests   = concatMap (genSpecTest preCondOps specOps) funcs

      (realtests,ignoredtests) = partition fst $
        if not (optProp opts)
        then []
        else concatMap (poly2default (optDefType opts)) $
               -- ignore already proved properties:
               filter (\fd -> funcName fd `notElem` map funcName theofuncs)
                      usertests ++
               (if optSpec opts then postCondTests ++ specOpTests else [])
  return (map snd realtests,
          map snd ignoredtests,
          CurryProg mname
                    (nub (easyCheckModule:imps))
                    dfltdecl clsdecls instdecls
                    typeDecls
                    (simpfuncs ++ map snd (realtests ++ ignoredtests))
                    opDecls)
 where
  (usertests, funcs) = partition isProperty functions


-- Extracts all determinism tests from a given Curry module and
-- transforms deterministic operations back into non-deterministic operations
-- in order to test their determinism property.
-- The result contains a triple consisting of all actual determinism tests,
-- all ignored tests (since they are polymorphic), and the public version
-- of the transformed original module.
transformDetTests :: Options -> [String] -> CurryProg
                  -> ([CFuncDecl],[CFuncDecl],CurryProg)
transformDetTests opts prooffiles
                  (CurryProg mname imports dfltdecl clsdecls instdecls
                             typeDecls functions opDecls) =
  (map snd realtests, map snd ignoredtests,
   CurryProg mname
             (nub (easyCheckModule:imports))
             dfltdecl clsdecls instdecls
             typeDecls
             (map (revertDetOpTrans detOpNames) functions ++
              map snd (realtests ++ ignoredtests))
             opDecls)
 where
  preCondOps = preCondOperations functions

  -- generate determinism tests:
  detOpTests = genDetOpTests prooffiles preCondOps functions

  -- names of deterministic operations:
  detOpNames = map (stripIsDet . funcName) detOpTests

  stripIsDet (mn,fn) = (mn, take (length fn -15) fn)

  (realtests,ignoredtests) = partition fst $
    if not (optProp opts)
    then []
    else concatMap (poly2default (optDefType opts))
                   (if optDet opts then detOpTests else [])

-- Get all operations with a defined precondition from a list of functions.
preCondOperations :: [CFuncDecl] -> [QName]
preCondOperations fdecls =
  map ((\ (mn,fn) -> (mn,fromPreCondName fn)) . funcName)
      (funDeclsWith isPreCondName fdecls)

-- Filter functions having a name satisfying a given predicate.
funDeclsWith :: (String -> Bool) -> [CFuncDecl] -> [CFuncDecl]
funDeclsWith pred = filter (pred . snd . funcName)

-- Transforms a function type into a property type, i.e.,
-- t1 -> ... -> tn -> t  is transformed into  t1 -> ... -> tn -> Prop
propResultType :: CTypeExpr -> CTypeExpr
propResultType te = case te of
  CFuncType from to -> CFuncType from (propResultType to)
  _                 -> baseType (easyCheckModule,"Prop")

-- Transforms a function declaration into a post condition test if
-- there is a post condition for this function (i.e., a relation named
-- f'post). The post condition test is of the form
-- fSatisfiesPostCondition x1...xn y = always (f'post x1...xn (f x1...xn))
genPostCondTest :: [QName] -> [QName] -> CFuncDecl -> [CFuncDecl]
genPostCondTest prefuns postops (CmtFunc _ qf ar vis texp rules) =
  genSpecTest prefuns postops (CFunc qf ar vis texp rules)
genPostCondTest prefuns postops
                (CFunc qf@(mn,fn) _ _ (CQualType clscon texp) _) =
 if qf `notElem` postops then [] else
  [CFunc (mn, fn ++ postCondSuffix) ar Public
    (CQualType clscon (propResultType texp))
    [simpleRule (map CPVar cvars) $
      if qf `elem` prefuns
       then applyF (easyCheckModule,"==>")
                   [applyF (mn,toPreCondName fn) (map CVar cvars), postprop]
       else postprop
    ]]
 where
  ar       = arityOfType texp
  cvars    = map (\i -> (i,"x"++show i)) [1 .. ar]
  rcall    = applyF qf (map CVar cvars)
  postprop = applyF (easyCheckModule,"always")
                    [applyF (mn,toPostCondName fn)
                            (map CVar cvars ++ [rcall])]

-- Transforms a function declaration into a specification test if
-- there is a specification for this function (i.e., an operation named
-- f'spec). The specification test is of the form
-- fSatisfiesSpecification x1...xn =
--   f'pre x1...xn  ==> (f x1...xn <~> f'spec x1...xn)
genSpecTest :: [QName] -> [QName] -> CFuncDecl -> [CFuncDecl]
genSpecTest prefuns specops (CmtFunc _ qf ar vis texp rules) =
  genSpecTest prefuns specops (CFunc qf ar vis texp rules)
genSpecTest prefuns specops
            (CFunc qf@(mn,fn) _ _ (CQualType clscon texp) _) =
 if qf `notElem` specops then [] else
  [CFunc (mn, fn ++ satSpecSuffix) ar Public
    (CQualType (addShowContext clscon) (propResultType texp))
    [simpleRule (map CPVar cvars) $
       addPreCond (applyF (easyCheckModule,"<~>")
                          [applyF qf (map CVar cvars),
                           applyF (mn,toSpecName fn) (map CVar cvars)])]]
 where
  cvars = map (\i -> (i,"x"++show i)) [1 .. ar]
  ar    = arityOfType texp

  addPreCond exp = if qf `elem` prefuns
                   then applyF (easyCheckModule,"==>")
                          [applyF (mn,toPreCondName fn) (map CVar cvars), exp]
                   else exp

-- Revert the transformation for deterministic operations performed
-- by currypp, i.e., replace rule "f x = selectValue (set f_ORGNDFUN x)"
-- with "f = f_ORGNDFUN".
revertDetOpTrans :: [QName] -> CFuncDecl -> CFuncDecl
revertDetOpTrans  detops (CmtFunc _ qf ar vis texp rules) =
  revertDetOpTrans detops (CFunc qf ar vis texp rules)
revertDetOpTrans detops fdecl@(CFunc qf@(mn,fn) ar vis texp _) =
  if qf `elem` detops
  then CFunc qf ar vis texp [simpleRule [] (constF (mn,fn++"_ORGNDFUN"))]
  else fdecl

-- Look for operations named f_ORGNDFUN and create a determinism property
-- for f.
genDetOpTests :: [String] -> [QName] -> [CFuncDecl] -> [CFuncDecl]
genDetOpTests prooffiles prefuns fdecls =
  map (genDetProp prefuns) (filter isDetOrgOp fdecls)
 where
  isDetOrgOp fdecl =
   let fn = snd (funcName fdecl)
    in "_ORGNDFUN" `isSuffixOf` fn &&
       not (existsProofFor (determinismTheoremFor (take (length fn - 9) fn))
                           prooffiles)

-- Transforms a declaration of a deterministic operation f_ORGNDFUN
-- into a determinisim property test of the form
-- fIsDeterministic x1...xn = f x1...xn #< 2
genDetProp :: [QName] -> CFuncDecl -> CFuncDecl
genDetProp prefuns (CmtFunc _ qf ar vis texp rules) =
  genDetProp prefuns (CFunc qf ar vis texp rules)
genDetProp prefuns (CFunc (mn,fn) ar _ (CQualType clscon texp) _) =
  CFunc (mn, forg ++ isDetSuffix) ar Public
   (CQualType (addShowContext clscon) (propResultType texp))
   [simpleRule (map CPVar cvars) $
      if (mn,forg) `elem` prefuns
       then applyF (easyCheckModule,"==>")
                   [applyF (mn,toPreCondName forg) (map CVar cvars), rnumcall]
       else rnumcall ]
 where
  forg     = take (length fn - 9) fn
  cvars    = map (\i -> (i,"x"++show i)) [1 .. ar]
  forgcall = applyF (mn,forg) (map CVar cvars)
  rnumcall = applyF (easyCheckModule,"#<") [forgcall, cInt 2]

-- Generates auxiliary (base-type instantiated) test functions for
-- polymorphically typed test function.
-- The flag indicates whether the test function should be actually passed
-- to the test tool.
poly2default :: String -> CFuncDecl -> [(Bool,CFuncDecl)]
poly2default dt (CmtFunc _ name arity vis ftype rules) =
  poly2default dt (CFunc name arity vis ftype rules)
poly2default dt fdecl@(CFunc (mn,fname) arity vis qftype rs)
  | isPolyType ftype
  = [(False,fdecl)
    ,(True, CFunc (mn,fname++defTypeSuffix) arity vis
                  (emptyClassType (p2dt ftype))
                  [simpleRule [] (applyF (mn,fname) [])])
    ]
  | otherwise
  = [(True, CFunc (mn,fname) arity vis (CQualType clscon ftype) rs)]
 where
  CQualType clscon ftype = defaultQualType qftype

  p2dt (CTVar _)         = baseType (pre dt)
  p2dt (CFuncType t1 t2) = CFuncType (p2dt t1) (p2dt t2)
  p2dt (CTApply t1 t2)   = CTApply (p2dt t1) (p2dt t2)
  p2dt (CTCons ct)       = CTCons ct

-------------------------------------------------------------------------
-- Try to default a qualified type by replacing Num/Integral-constrained
-- types by Int and Fractional-constrained types by Float.
defaultQualType :: CQualTypeExpr -> CQualTypeExpr
defaultQualType (CQualType (CContext allclscon) ftype) =
  CQualType (CContext deffractxt) deffratype
 where
  (numcons,nonnumcons) =
    partition (\ (cls,te) -> (cls == pre "Num" || cls == pre "Integral")
                             && isTVar te)
              allclscon
  defnumtype = def2TConsInType numcons (pre "Int") ftype
  defnumctxt = removeNonTVarClassContexts
                 (map (\ (cls,con) ->
                                (cls, def2TConsInType numcons (pre "Int") con))
                      nonnumcons)

  (fracons,nonfracons) =
    partition (\ (cls,te) -> cls == pre "Fractional" && isTVar te) defnumctxt
  deffratype = def2TConsInType fracons (pre "Float") defnumtype
  deffractxt = removeNonTVarClassContexts
                 (map (\ (cls,con) ->
                              (cls, def2TConsInType fracons (pre "Float") con))
                      nonfracons)

  -- remove constant type class contexts
  removeNonTVarClassContexts = filter (\ (_,te) -> isTVar te)

  -- replace all type variables (occurring in the first list of class
  -- constraints) by the type constructor (second argument) in a given
  -- type expression (third argument)
  def2TConsInType clscons tcons texp =
    foldr (tvar2TCons tcons) texp (map snd clscons)

  -- substitute a type variable by type Int in a type
  tvar2TCons tcons texp = case texp of
    CTVar tv -> substTVar tv (CTCons tcons)
    _        -> id

  -- substitute a type variable by a type expression in a type expression:
  substTVar tvariname texp =
    trCTypeExpr (\tv -> if tv==tvariname then texp else CTVar tv)
                CTCons CFuncType CTApply

  isTVar te = case te of CTVar _ -> True
                         _       -> False

-- Add a "Show" class context to all types occurring in the context.
addShowContext :: CContext -> CContext
addShowContext (CContext clscons) =
  CContext (nub (clscons ++ (map (\t -> (pre "Show",t)) (map snd clscons))))

-------------------------------------------------------------------------

-- Transforms a possibly changed test name (like "test_ON_BASETYPE")
-- back to its original name.
orgTestName :: QName -> QName
orgTestName (mn,tname)
  | defTypeSuffix `isSuffixOf` tname
  = orgTestName (mn, stripSuffix tname defTypeSuffix)
  | isDetSuffix `isSuffixOf` tname
  = orgTestName (mn, take (length tname - 15) tname)
  | postCondSuffix `isSuffixOf` tname
  = orgTestName (mn, stripSuffix tname postCondSuffix)
  | satSpecSuffix `isSuffixOf` tname
  = orgTestName (mn, stripSuffix tname satSpecSuffix)
  | otherwise = (mn,tname)

-- This function implements the first phase of CurryCheck: it analyses
-- a module to be checked, i.e., it finds the tests, creates new tests
-- (e.g., for polymorphic properties, deterministic functions, post
-- conditions, specifications)
-- and generates a copy of the module appropriate for the main operation
-- of CurryCheck (e.g., all operations are made public).
-- If there are determinism tests, it also generates a second copy
-- where all deterministic functions are defined as non-deterministic
-- so that these definitions are tested.
analyseModule :: Options -> String -> IO [TestModule]
analyseModule opts modname = do
  putStrIfNormal opts $ withColor opts blue $
                        "Analyzing module '" ++ modname ++ "'...\n"
  catch (readCurryWithParseOptions modname (setQuiet True defaultParams) >>=
         analyseCurryProg opts modname)
        (\_ -> return [staticErrorTestMod modname
                         ["Module '"++modname++"': incorrect source program"]])

-- Analyse a Curry module for static errors:
staticProgAnalysis :: Options -> String -> String -> CurryProg
                   -> IO ([String],[(QName,String)])
staticProgAnalysis opts modname progtxt prog = do
  putStrIfVerbose opts "Checking source code for static errors...\n"
  useerrs <- if optSource opts then checkBlacklistUse prog else return []
  seterrs <- if optSource opts then readFlatCurry modname >>= checkSetUse
                               else return []
  let defruleerrs = if optSource opts then checkDefaultRules prog else []
  untypedprog <- readUntypedCurry modname
  let detuseerrs   = if optSource opts then checkDetUse untypedprog else []
      contracterrs = checkContractUse prog
      staticerrs = concat [seterrs,useerrs,defruleerrs,detuseerrs,contracterrs]
      missingCPP =
       if (containsDefaultRules prog || containsDetOperations untypedprog)
          && not (containsPPOptionLine progtxt)
       then ["'" ++ modname ++
           "' uses default rules or det. operations but not the preprocessor!",
           "Hint: insert line: {-# OPTIONS_CYMAKE -F --pgmF=currypp #-}"]
       else []
  return (missingCPP,staticerrs)

-- Analyse a Curry module and generate property tests:
analyseCurryProg :: Options -> String -> CurryProg -> IO [TestModule]
analyseCurryProg opts modname orgprog = do
  -- First we rename all references to Test.Prop into Test.EasyCheck
  let prog = renameProp2EasyCheck orgprog
  (topdir,srcfilename) <- lookupModuleSourceInLoadPath modname >>=
        return .
        maybe (error $ "Source file of module '"++modname++"' not found!") id
  let srcdir = takeDirectory srcfilename
  putStrLnIfDebug opts $ "Source file: " ++ srcfilename
  prooffiles <- if optProof opts then getProofFiles srcdir else return []
  unless (null prooffiles) $ putStrIfVerbose opts $
    unlines ("Proof files found:" : map ("- " ++) prooffiles)
  progtxt <- readFile srcfilename
  (missingCPP,staticoperrs) <- staticProgAnalysis opts modname progtxt prog
  let words      = map firstWord (lines progtxt)
      staticerrs = missingCPP ++ map (showOpError words) staticoperrs
  putStrIfVerbose opts "Generating property tests...\n"
  (rawTests,ignoredTests,pubmod) <-
        transformTests opts srcdir . renameCurryModule (modname++"_PUBLIC")
                                   . makeAllPublic $ prog
  let (rawDetTests,ignoredDetTests,pubdetmod) =
        transformDetTests opts prooffiles
              . renameCurryModule (modname++"_PUBLICDET")
              . makeAllPublic $ prog
  unless (not (null staticerrs) || null rawTests && null rawDetTests) $
    putStrIfNormal opts $
      "Properties to be tested:\n" ++
      unwords (map (snd . funcName) (rawTests++rawDetTests)) ++ "\n"
  unless (not (null staticerrs) || null ignoredTests && null ignoredDetTests) $
    putStrIfNormal opts $
      "Properties ignored for testing:\n" ++
      unwords (map (snd . funcName) (ignoredTests++ignoredDetTests)) ++ "\n"
  let tm    = TestModule modname
                         (progName pubmod)
                         staticerrs
                         (addLinesNumbers words (classifyTests rawTests))
                         (generatorsOfProg pubmod)
      dettm = TestModule modname
                         (progName pubdetmod)
                         []
                         (addLinesNumbers words (classifyTests rawDetTests))
                         (generatorsOfProg pubmod)
  when (testThisModule tm) $ writeCurryProgram opts topdir pubmod ""
  when (testThisModule dettm) $ writeCurryProgram opts topdir pubdetmod ""
  return (if testThisModule dettm then [tm,dettm] else [tm])
 where
  showOpError words (qf,err) =
    snd qf ++ " (module " ++ modname ++ ", line " ++
    show (getLineNumber words qf) ++"): " ++ err

  addLinesNumbers words = map (addLineNumber words)

  addLineNumber :: [String] -> Test -> Test
  addLineNumber words (PropTest   name texp _) =
    PropTest   name texp $ getLineNumber words (orgTestName name)
  addLineNumber words (IOTest name _) =
    IOTest name $ getLineNumber words (orgTestName name)

  getLineNumber :: [String] -> QName -> Int
  getLineNumber words (_, name) = maybe 0 (+1) (elemIndex name words)

-- Extracts all user-defined defined generators defined in a module.
generatorsOfProg :: CurryProg -> [QName]
generatorsOfProg = map funcName . filter isGen . functions
 where
   isGen fdecl = "gen" `isPrefixOf` snd (funcName fdecl) &&
                 isSearchTreeType (resultType (typeOfQualType (funcType fdecl)))

   isSearchTreeType (CTVar _) = False
   isSearchTreeType (CFuncType _ _) = False
   isSearchTreeType (CTApply _ _) = False
   isSearchTreeType (CTCons tc) = tc == searchTreeTC -- TODO!

-------------------------------------------------------------------------
-- Create the main test module containing all tests of all test modules as
-- a Curry program with name `mainmodname`.
-- The main test module contains a wrapper operation for each test
-- and a main function to execute these tests.
-- Furthermore, if PAKCS is used, test data generators
-- for user-defined types are automatically generated.
genMainTestModule :: Options -> String -> [TestModule] -> IO ()
genMainTestModule opts mainmodname modules = do
  let testtypes = nub (concatMap userTestDataOfModule modules)
  testtypedecls <- collectAllTestTypeDecls [] testtypes
  let generators   = map (createTestDataGenerator mainmodname) testtypedecls
      funcs        = concatMap (createTests opts mainmodname) modules ++
                               generators
      mainFunction = genMainFunction opts mainmodname
                                     (concatMap propTests modules)
      imports      = nub $ [ easyCheckModule, easyCheckExecModule
                           , searchTreeModule, generatorModule
                           , "AnsiCodes","Maybe","System"] ++
                           map fst testtypes ++ map testModuleName modules
  appendix <- readFile (packagePath </> "src" </> "TestAppendix.curry")
  writeCurryProgram opts "."
    (CurryProg mainmodname imports Nothing [] [] [] (mainFunction : funcs) [])
    appendix

-- Generates the main function which executes all property tests
-- of all test modules.
genMainFunction :: Options -> String -> [Test] -> CFuncDecl
genMainFunction opts testModule tests =
  CFunc (testModule, "main") 0 Public (emptyClassType (ioType unitType))
        [simpleRule [] body]
 where
  body = CDoExpr $
     (if isQuiet opts
        then []
        else [CSExpr (applyF (pre "putStrLn")
                             [string2ac "Executing all tests..."])]) ++
     [ CSPat (cpvar "x1") $ -- run all tests:
          applyF (testModule, "runPropertyTests")
                 [constF (pre (if optColor opts then "True" else "False")),
                  easyCheckExprs]
     , CSExpr $ applyF (pre "when")
                  [applyF (pre "/=") [cvar "x1", cInt 0],
                   applyF ("System", "exitWith") [cvar "x1"]]
     ]

  easyCheckExprs = list2ac $ map makeExpr tests

  makeExpr :: Test -> CExpr
  makeExpr (PropTest (mn, name) _ _) =
    constF (testModule, name ++ "_" ++ modNameToId mn)
  makeExpr (IOTest (mn, name) _) =
    constF (testModule, name ++ "_" ++modNameToId  mn)

-------------------------------------------------------------------------
-- Collect all type declarations for a given list of type
-- constructor names, including the type declarations which are
-- used in these type declarations.
collectAllTestTypeDecls :: [FC.TypeDecl] -> [QName] -> IO [FC.TypeDecl]
collectAllTestTypeDecls tdecls testtypenames = do
  newtesttypedecls <- mapIO getTypeDecl testtypenames
  let alltesttypedecls = tdecls ++ newtesttypedecls
      newtcons = filter (\ (mn,_) -> mn /= preludeName)
                        (nub (concatMap allTConsInDecl newtesttypedecls)
                         \\ map FCG.typeName alltesttypedecls)
  if null newtcons then return alltesttypedecls
                   else collectAllTestTypeDecls alltesttypedecls newtcons
 where
  -- gets the type declaration for a given type constructor
  -- (could be improved by caching programs that are already read)
  getTypeDecl :: QName -> IO FC.TypeDecl
  getTypeDecl qt@(mn,_) = do
    fprog <- readFlatCurry mn
    maybe (error $ "Definition of type '" ++ FC.showQNameInModule "" qt ++
                   "' not found!")
          return
          (find (\t -> FCG.typeName t == qt) (FCG.progTypes fprog))

  -- compute all type constructors used in a type declaration
  allTConsInDecl :: FC.TypeDecl -> [QName]
  allTConsInDecl = FCG.trType (\_ _ _ -> concatMap allTConsInConsDecl)
                              (\_ _ _ -> allTConsInTypeExpr)

  allTConsInConsDecl :: FC.ConsDecl -> [QName]
  allTConsInConsDecl = FCG.trCons (\_ _ _ -> concatMap allTConsInTypeExpr)

  allTConsInTypeExpr :: FC.TypeExpr -> [QName]
  allTConsInTypeExpr =
    FCG.trTypeExpr (\_ -> []) (\tc targs -> tc : concat targs) (++) (flip const)

-- Creates a test data generator for a given type declaration.
createTestDataGenerator :: String -> FC.TypeDecl -> CFuncDecl
createTestDataGenerator mainmodname tdecl = type2genData tdecl
 where
  qt       = FCG.typeName tdecl
  qtString = FC.showQNameInModule "" qt

  type2genData (FC.TypeSyn _ _ _ _) =
    error $ "Cannot create generator for type synonym " ++ qtString
  type2genData (FC.Type _ _ tvars cdecls) =
    if null cdecls
    then error $ "Cannot create value generator for type '" ++ qtString ++
                 "' without constructors!"
    else CFunc (typename2genopname mainmodname [] qt) (length tvars) Public
               (emptyClassType
                 (foldr (~>) (CTApply (CTCons searchTreeTC)
                                      (applyTC qt ctvars))
                             (map (\v -> applyTC searchTreeTC [v]) ctvars)))
               [simpleRule (map CPVar cvars)
                  (foldr1 (\e1 e2 -> applyF (generatorModule,"|||") [e1,e2])
                          (map cons2gen cdecls))]
   where
    cons2gen (FC.Cons qn ar _ ctypes) =
      if ar>maxArity
      then error $ "Test data constructors with more than " ++ show maxArity ++
                   " arguments are currently not supported!"
      else applyF (generatorModule, "genCons" ++ show ar)
                  ([CSymbol qn] ++ map type2gen ctypes)

    type2gen (FC.TVar i) = CVar (i,"a"++show i)
    type2gen (FC.FuncType _ _) =
      error $ "Type '" ++ qtString ++
              "': cannot create value generators for functions!"
    type2gen (FC.TCons qtc argtypes) =
      applyF (typename2genopname mainmodname [] qtc) (map type2gen argtypes)
    type2gen (FC.ForallType _ _) =
      error $ "Type '" ++ qtString ++
              "': cannot create value generators for forall types!"

    ctvars = map (\i -> CTVar (i,"a"++show i)) tvars
    cvars  = map (\i -> (i,"a"++show i)) tvars

-------------------------------------------------------------------------
-- remove the generated files (except if option "--keep" is set)
cleanup :: Options -> String -> [TestModule] -> IO ()
cleanup opts mainmodname modules =
  unless (optKeep opts) $ do
    removeCurryModule mainmodname
    mapIO_ removeCurryModule (map testModuleName modules)
 where
  removeCurryModule modname = do
    (_,srcfilename) <- lookupModuleSourceInLoadPath modname >>=
        return .
        maybe (error $ "Source file of module '"++modname++"' not found!") id
    system $ installDir </> "bin" </> "cleancurry" ++ " " ++ modname
    system $ "rm -f " ++ srcfilename

-- Show some statistics about number of tests:
showTestStatistics :: [TestModule] -> String
showTestStatistics testmodules =
  let numtests  = sumOf (const True) testmodules
      unittests = sumOf isUnitTest   testmodules
      proptests = sumOf isPropTest   testmodules
      iotests   = sumOf isIOTest     testmodules
   in "TOTAL NUMBER OF TESTS: " ++ show numtests ++
      " (UNIT: " ++ show unittests ++ ", PROPERTIES: " ++
      show proptests ++ ", IO: " ++ show iotests ++ ")"
 where
  sumOf p = foldr (+) 0 . map (length . filter p . propTests)

-------------------------------------------------------------------------
main :: IO ()
main = do
  argv <- getArgs
  pid  <- getPID
  let (funopts, args, opterrors) = getOpt Permute options argv
  opts <- processOpts (foldl (flip id) defaultOptions funopts)
  unless (null opterrors)
         (putStr (unlines opterrors) >> putStrLn usageText >> exitWith 1)
  putStrIfNormal opts ccBanner
  when (null args || optHelp opts) (putStrLn usageText >> exitWith 1)
  let mods = map stripCurrySuffix args
  mapIO_ checkModuleName mods
  testModules <- mapIO (analyseModule opts) mods
  let staticerrs       = concatMap staticErrors (concat testModules)
      finaltestmodules = filter testThisModule (concat testModules)
      testmodname = if null (optMainProg opts)
                      then "TEST" ++ show pid
                      else optMainProg opts
  if not (null staticerrs)
   then do showStaticErrors opts staticerrs
           putStrLn $ withColor opts red "Testing aborted!"
           cleanup opts testmodname finaltestmodules
           exitWith 1
   else if null finaltestmodules then exitWith 0 else do
    putStrIfNormal opts $ withColor opts blue $
                          "Generating main test module '"++testmodname++"'..."
    genMainTestModule opts testmodname finaltestmodules
    showGeneratedModule opts "main test" testmodname
    putStrIfNormal opts $ withColor opts blue $ "and compiling it...\n"
    ecurrypath <- getEnviron "CURRYPATH"
    let currypath = case ecurrypath of ':':_ -> '.':ecurrypath
                                       _     -> ecurrypath
    let runcmd = unwords $
                   [ installDir </> "bin" </> "curry"
                   , "--noreadline"
                   , ":set -time"
                   , ":set " ++ if optVerb opts > 3 then "v1" else "v0"
                   , ":set parser -Wnone"
                   , if null currypath then "" else ":set path " ++ currypath
                   , ":l "++testmodname,":eval main :q" ]
    putStrLnIfDebug opts $ "Executing command:\n" ++ runcmd
    ret <- system runcmd
    cleanup opts testmodname finaltestmodules
    unless (isQuiet opts || ret /= 0) $
      putStrLn $ withColor opts green $ showTestStatistics finaltestmodules
    exitWith ret
 where
  showStaticErrors opts errs = putStrLn $ withColor opts red $
    unlines (line : "STATIC ERRORS IN PROGRAMS:" : errs) ++ line

  checkModuleName mn =
    when (pathSeparator `elem` mn) $ do
      putStrLn $ "Module names with path prefixes not allowed: " ++ mn
      exitWith 1

  line = take 78 (repeat '=')

showGeneratedModule :: Options -> String -> String -> IO ()
showGeneratedModule opts mkind modname = when (optVerb opts > 3) $ do
  putStrLn $ '\n' : line
  putStrLn $ "Generated " ++ mkind ++ " module `" ++ modname ++ ".curry':"
  putStrLn line
  readFile (modname ++ ".curry") >>= putStr
  putStrLn line
 where
  line = take 78 (repeat '=')

-------------------------------------------------------------------------
-- Auxiliaries

-- Rename all module references to "Test.Prog" into "Test.EasyCheck"
renameProp2EasyCheck :: CurryProg -> CurryProg
renameProp2EasyCheck prog =
  updCProg id (map rnmMod) id id id id id id
           (updQNamesInCProg (\ (mod,n) -> (rnmMod mod,n)) prog)
 where
  rnmMod mod | mod == propModule = easyCheckModule
             | otherwise         = mod

-- Extracts the first word of a string
firstWord :: String -> String
firstWord = head . splitOn "\t" . head . splitOn " "

-- Strips a suffix from a string.
stripSuffix :: String -> String -> String
stripSuffix str suf = if suf `isSuffixOf` str
                      then take (length str - length suf) str
                      else str

-- Translate a module name to an identifier, i.e., replace '.' by '_':
modNameToId :: String -> String
modNameToId = intercalate "_" . split (=='.')

-- Computes the arity from a type expression.
arityOfType :: CTypeExpr -> Int
arityOfType = length . argTypes

--- Name of the SearchTree module.
searchTreeModule :: String
searchTreeModule = "SearchTree"

--- Name of SearchTree type constructor.
searchTreeTC :: QName
searchTreeTC = (searchTreeModule,"SearchTree")

--- Name of the SearchTreeGenerator module.
generatorModule :: String
generatorModule = "SearchTreeGenerators"

-- Writes a Curry module (together with an appendix) to its file.
writeCurryProgram :: Options -> String -> CurryProg -> String -> IO ()
writeCurryProgram opts srcdir p appendix = do
  let progfile = srcdir </> modNameToPath (progName p) ++ ".curry"
  putStrLnIfDebug opts $ "Writing program: " ++ progfile
  writeFile progfile
            (showCProg p ++ "\n" ++ appendix ++ "\n")

isPAKCS :: Bool
isPAKCS = curryCompiler == "pakcs"

-- Does a program text contains a OPTIONS_CYMAKE line to call currypp?
containsPPOptionLine :: String -> Bool
containsPPOptionLine = any isOptionLine . lines
 where
   isOptionLine s = "{-# OPTIONS_CYMAKE " `isPrefixOf` s -- -}
                    && "currypp" `isInfixOf` s

-------------------------------------------------------------------------
