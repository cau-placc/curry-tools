-----------------------------------------------------------------------------
--- Required value analysis for Curry programs
---
--- This analysis checks for each function in a Curry program whether
--- the arguments of a function must have a particular shape in order to
--- compute some value of this function.
--- For instance, the negation operation `not` requires the argument
--- value `False` in order to compute the result `True` and it requires
--- the argument `True` to compute the result `False`.
---
--- @author Michael Hanus
--- @version April 2018
-----------------------------------------------------------------------------

module Analysis.RequiredValue
  (AType(..), showAType, AFType(..), showAFType, lubAType, reqValueAnalysis)
 where

import Analysis.Types
import Analysis.ProgInfo
import Analysis.TotallyDefined(siblingCons)

import FlatCurry.Types
import FlatCurry.Goodies
import List
import Sort(mergeSortBy)


------------------------------------------------------------------------------
-- Our abstract (non-standard) type domain.
-- `Any` represents any expression,
-- `AnyC` represents any value (i.e., constructor-rooted term),
-- `Cons c` a value rooted by the constructor `c`, and
-- `Empty` represents no possible value.
data AType = Any | AnyC | Cons QName | Empty
 deriving (Eq,Ord)

--- Is some abstract type a constructor?
isConsValue :: AType -> Bool
isConsValue av = case av of Cons _ -> True
                            _      -> False

--- Least upper bound of abstract values.
lubAType :: AType -> AType -> AType
lubAType Any      _        = Any
lubAType AnyC     Any      = Any
lubAType AnyC     AnyC     = AnyC
lubAType AnyC     (Cons _) = AnyC
lubAType AnyC     Empty    = AnyC
lubAType (Cons _) Any      = Any
lubAType (Cons _) AnyC     = AnyC
lubAType (Cons c) (Cons d) = if c==d then Cons c else AnyC
lubAType (Cons c) Empty    = Cons c
lubAType Empty    av       = av

--- Join two abstract values. The result is `Empty` if they are not compatible.
joinAType :: AType -> AType -> AType
joinAType Any      av       = av
joinAType AnyC     Any      = AnyC
joinAType AnyC     AnyC     = AnyC
joinAType AnyC     (Cons c) = Cons c
joinAType AnyC     Empty    = Empty
joinAType (Cons c) Any      = Cons c
joinAType (Cons c) AnyC     = Cons c
joinAType (Cons c) (Cons d) = if c==d then Cons c else Empty
joinAType (Cons _) Empty    = Empty
joinAType Empty    _        = Empty

--- Are two abstract types compatible, i.e., describe common values?
compatibleType :: AType -> AType -> Bool
compatibleType t1 t2 = joinAType t1 t2 /= Empty

-- Shows an abstract value.
showAType :: AOutFormat -> AType -> String
showAType _ Any  = "any"
showAType _ AnyC = "cons"
showAType _ (Cons (_,n)) = n --q++"."++n
showAType _ Empty = "_|_"

--- The abstract type of a function.
--- It is either `EmptyFunc`, i.e., contains no information about
--- the possible result of the function,
--- or a list of possible argument/result type pairs.
data AFType = EmptyFunc | AFType [([AType],AType)]
 deriving Eq

-- Shows an abstract value.
showAFType :: AOutFormat -> AFType -> String
showAFType _ EmptyFunc = "EmptyFunc"
showAFType aof (AFType fts) = intercalate " | " (map showFType fts)
 where
  showFType (targs,tres) =
    "(" ++ intercalate "," (map (showAType aof) targs) ++ " -> " ++
           showAType aof tres ++ ")"

showCalledFuncs :: [(QName,AFType)] -> String
showCalledFuncs =
  intercalate "|" . map (\ ((_,f),at) -> f++"::"++showAFType _ at)

------------------------------------------------------------------------------
--- An abstract environments used in the analysis of a function associates
--- to each variable (index) an abstract type.
type AEnv = [(Int,AType)]

--- Extend an abstract environment with variables of any type:
extendEnv :: AEnv -> [Int] -> AEnv
extendEnv env vars = zip vars (repeat Any) ++ env

--- Update a variable in an abstract environment:
updateVarInEnv :: AEnv -> Int -> AType -> AEnv 
updateVarInEnv [] v _ = error ("Variable "++show v++" not found in environment")
updateVarInEnv ((i,ov):env) v nv =
  if i==v then (i,nv) : env
          else (i,ov) : updateVarInEnv env v nv

--- Drop the first n elements from the environment component
--- of an environment/type pair:
dropEnv :: Int -> ([a],b) -> ([a],b)
dropEnv n (env,rtype) = (drop n env, rtype)

-- Sorts a list of environment/type pairs by the type.
sortEnvTypes :: [(AEnv,AType)] -> [(AEnv,AType)]
sortEnvTypes = mergeSortBy (\ (e1,t1) (e2,t2) -> (t1,e1) <= (t2,e2))

------------------------------------------------------------------------------
--- The maximum number of different constructors considered for the
--- required value analysis. If a type has more constructors than
--- specified here, it will not be analyzed for individual required
--- constructor values.
maxReqValues :: Int
maxReqValues = 3

--- Required value analysis.
reqValueAnalysis :: Analysis AFType
reqValueAnalysis =
  combinedDependencyFuncAnalysis "RequiredValue"
                                 siblingCons EmptyFunc analyseReqVal

analyseReqVal :: ProgInfo [(QName,Int)] -> FuncDecl -> [(QName,AFType)]
              -> AFType
analyseReqVal consinfo (Func (m,f) arity _ _ rule) calledfuncs
 | m==prelude = maybe (anaresult rule) id (lookup f preludeFuncs)
 | otherwise  = --trace ("Analyze "++f++"\n"++showCalledFuncs calledfuncs++
                --       "\nRESULT: "++showAFType _ (anaresult rule)) $
                anaresult rule
 where
  anaresult (External _) = AFType [(take arity (repeat Any),AnyC)]
  anaresult (Rule args rhs) = analyseReqValRule consinfo calledfuncs args rhs

  -- add special results for prelude functions here:
  preludeFuncs = [("failed",AFType [([],Empty)])
                 ,("==",AFType [([AnyC,AnyC],AnyC)])
                 ,("=:=",AFType [([AnyC,AnyC],AnyC)])
                 ,("$",AFType [([AnyC,Any],AnyC)])
                 ,("$!",AFType [([AnyC,AnyC],AnyC)])
                 ,("$!!",AFType [([AnyC,AnyC],AnyC)])
                 ,("$#",AFType [([AnyC,AnyC],AnyC)])
                 ,("$##",AFType [([AnyC,AnyC],AnyC)])
                 ,("compare",AFType [([AnyC,AnyC],AnyC)])
                 ]

analyseReqValRule :: ProgInfo [(QName,Int)] -> [(QName,AFType)] -> [Int] -> Expr
                  -> AFType
analyseReqValRule consinfo calledfuncs args rhs =
  let initenv = extendEnv [] args
      envtypes = reqValExp initenv rhs AnyC
      rtypes = map snd envtypes
   in -- If some result is `AnyC` and another result is a constructor, then
      -- analyze again for all constructors as required results
      -- in order to get more precise information.
      if any (==AnyC) rtypes && any isConsValue rtypes
      then
       let somecons = maybe (error "Internal error")
                                (\ (Cons c) -> c)
                                (find isConsValue rtypes)
           othercons = maybe [] (map fst) (lookupProgInfo somecons consinfo)
           consenvtypes = foldr lubEnvTypes []
                                (map (\rt -> reqValExp initenv rhs rt)
                                     (map Cons (somecons:othercons)))
        in AFType (map (\ (env,rtype) -> (map snd env, rtype))
                       (lubAnyEnvTypes (if length othercons >= maxReqValues
                                        then envtypes
                                        else consenvtypes)))
      else AFType (map (\ (env,rtype) -> (map snd env, rtype))
                       (lubAnyEnvTypes envtypes))
 where
  reqValExp env exp reqtype = case exp of
    Var v -> [(updateVarInEnv env v reqtype, reqtype)]
    Lit _ -> [(env, AnyC)] -- too many literal constants...
    Comb ConsCall c _ -> [(env, Cons c)] -- analysis of arguments superfluous
    Comb FuncCall qf funargs ->
      if qf==(prelude,"?") && length funargs == 2
      then -- use intended definition of Prelude.? for more precise analysis:
           reqValExp env (Or (head funargs) (funargs!!1)) reqtype
      else
        maybe [(env, AnyC)]
              (\ftype -> case ftype of
                 EmptyFunc -> [(env, Empty)] -- no information available
                 AFType ftypes ->
                   let matchingtypes = filter (compatibleType reqtype . snd)
                                              ftypes
                       -- for all matching types analyze arguments
                       -- where a constructor value is required:
                       matchingenvs =
                         map (\ (ts,rt) ->
                              let argenvs =  concatMap (envForConsArg env)
                                                       (zip ts funargs)
                               in (foldr joinEnv env argenvs, rt))
                             matchingtypes
                    in if null matchingtypes
                       then [(env, Empty)]
                       else matchingenvs )
              (lookup qf calledfuncs)
    Comb _ _ _ -> [(env, AnyC)] -- no reasonable info for partial applications
    Or e1 e2 -> lubEnvTypes (reqValExp env e1 reqtype)
                            (reqValExp env e2 reqtype)
    Case _ e branches ->
      let -- filter non-failing branches:
          nfbranches = filter (\ (Branch _ be) ->
                                   be /=  Comb FuncCall (prelude,"failed") [])
                              branches
          reqenvs = filter (not . null)
                           (map (envForBranch env reqtype e) nfbranches)
       in if null reqenvs
          then [(env, Empty)]
          else foldr1 lubEnvTypes reqenvs
    Free vars e ->
      map (dropEnv (length vars))
          (reqValExp (extendEnv env vars) e reqtype)
    Let bindings e ->
      -- bindings are not analyzed since we don't know whether they are used:
      map (dropEnv (length bindings))
          (reqValExp (extendEnv env (map fst bindings)) e reqtype)
    Typed e _ -> reqValExp env e reqtype

  -- compute an expression environment for a function argument if this
  -- argument is required to be a constructor:
  envForConsArg env (reqtype,exp) =
    case reqtype of
      AnyC    -> [foldr1 lubEnv (map fst (reqValExp env exp AnyC))]
      Cons qc -> [foldr1 lubEnv (map fst (reqValExp env exp (Cons qc)))]
      _       -> []

  -- compute an expression environment and required type for an applied branch
  envForBranch env reqtype exp (Branch pat bexp) =
    filter (\ (_,rt) -> compatibleType rt reqtype) branchtypes
   where
    branchtypes = case pat of
      LPattern _   -> reqValExp env bexp reqtype
      Pattern qc pvars ->
        let caseenvs = map fst (reqValExp env exp (Cons qc))
            branchenvs =
              foldr lubEnvTypes []
                    (map (\caseenv ->
                             reqValExp (extendEnv caseenv pvars) bexp reqtype)
                         caseenvs)
         in map (dropEnv (length pvars)) branchenvs

--- "lub" two environment lists. All environment lists are ordered
--- by the result type.
lubEnvTypes :: [(AEnv,AType)] -> [(AEnv,AType)] -> [(AEnv,AType)]
lubEnvTypes []         ets2 = ets2
lubEnvTypes ets1@(_:_) []   = ets1
lubEnvTypes ((env1,t1):ets1) ((env2,t2):ets2)
  | t1==Empty = lubEnvTypes ets1 ((env2,t2):ets2) -- ignore "empty" infos
  | t2==Empty = lubEnvTypes ((env1,t1):ets1) ets2
  | t1==t2    = (lubEnv env1 env2, t1) : lubEnvTypes ets1 ets2
  | t1<t2     = (env1,t1) : lubEnvTypes ets1 ((env2,t2):ets2)
  | otherwise = (env2,t2) : lubEnvTypes ((env1,t1):ets1) ets2

--- "lub" the environments of the more specific types to the AnyC type
--- (if present).
lubAnyEnvTypes :: [(AEnv,AType)] -> [(AEnv,AType)]
lubAnyEnvTypes envtypes =
  if null envtypes || snd (head envtypes) /= AnyC
      then envtypes
      else foldr1 lubEnvType envtypes : tail envtypes

lubEnvType :: (AEnv,AType) -> (AEnv,AType) -> (AEnv,AType)
lubEnvType (env1,t1) (env2,t2) = (lubEnv env1 env2, lubAType t1 t2)

lubEnv :: AEnv -> AEnv -> AEnv
lubEnv []     _ = []
lubEnv (_:_) [] = []
lubEnv ((i1,v1):env1) env2@(_:_) =
  maybe (lubEnv env1 env2)
        (\v2 -> (i1, lubAType v1 v2) : lubEnv env1 env2)
        (lookup i1 env2)

joinEnv :: AEnv -> AEnv -> AEnv
joinEnv []     _ = []
joinEnv (_:_) [] = []
joinEnv ((i1,v1):env1) env2@(_:_) =
  maybe (joinEnv env1 env2)
        (\v2 -> (i1, joinAType v1 v2) : joinEnv env1 env2)
        (lookup i1 env2)

-- Name of the standard prelude:
prelude :: String
prelude = "Prelude"
