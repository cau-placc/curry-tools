------------------------------------------------------------------------
--- This module contains the implementation of the "postcondition" reducer
--- which simplifies the postconditions in a list of function declarations
--- w.r.t. a given list of theorems.
---
--- Note that theorems are standard (EasyCheck) properties having the
--- prefix `theorem'`, as
---
---      theorem'sortlength xs = length xs <~> length (sort xs)
---      
---      theorem'sorted xs = always (sorted (sort xs))
---
--- @author Michael Hanus
--- @version October 2016
------------------------------------------------------------------------

module SimplifyPostConds
  ( simplifyPostConditionsWithTheorems)
 where

import AbstractCurry.Types
import AbstractCurry.Select
import AbstractCurry.Build
import List                 (last, maximum)
import Maybe                (maybeToList)
import ReadShowTerm         (readQTerm)
import Rewriting.Files
import Rewriting.Term
import Rewriting.Position
import Rewriting.Substitution
import Rewriting.Rules

import ContractUsage
import TheoremUsage

-- Simplify the postconditions contained in the third argument w.r.t. a given
-- list of theorems (second argument).
-- If the verbosity (first argument) is greater than 1, the details
-- about the theorems, simplifcation rules, and reduced postconditions
-- are shown.
simplifyPostConditionsWithTheorems :: Int -> [CFuncDecl] -> [CFuncDecl]
                                   -> IO [CFuncDecl]
simplifyPostConditionsWithTheorems verb theofuncs postconds = do
  theoTRS <- mapIO safeFromFuncDecl theofuncs >>= return . concat
  if null theoTRS
    then return postconds
    else do
     let simprules = concatMap theoremToSimpRules theoTRS ++ standardSimpRules
     when (verb>1) $ putStr $ unlines
       [ "THEOREMS (with existing proof files):", showTRS snd theoTRS,
         "SIMPLIFICATION RULES (for postcondition reduction):"
       , showTRS snd simprules]
     simppostconds <- mapIO (safeSimplifyPostCondition verb simprules) postconds
     return (concat simppostconds)
 where
  safeFromFuncDecl fd =
    catch (return $!! (snd (fromFuncDecl fd)))
          (\e -> do
            putStrLn $ showError e ++ "\nTheorem \"" ++
                       snd (funcName fd) ++
                       "\" not used for simplification (too complex)."
            return [])

-- Simplify a single postcondition (third argument) w.r.t. a given
-- list of theorems (second argument):
safeSimplifyPostCondition :: Int -> TRS QName ->  CFuncDecl ->  IO [CFuncDecl]
safeSimplifyPostCondition verb simprules fundecl =
  catch (simplifyPostCondition verb simprules fundecl)
        (\e -> do putStrLn $ showError e ++ "\nPostcondition \"" ++
                             snd (funcName fundecl) ++
                             "\" not simplified (too complex)."
                  return [fundecl])

simplifyPostCondition :: Int -> TRS QName ->  CFuncDecl ->  IO [CFuncDecl]
simplifyPostCondition verb simprules (CFunc qn ar vis texp rs) =
  simplifyPostCondition verb simprules (CmtFunc "" qn ar vis texp rs)
simplifyPostCondition verb simprules fdecl@(CmtFunc cmt qn ar vis texp rules) =
  if isPostCondName (snd qn)
    then do redrules <- mapIO (simplifyRule verb simprules qn) rules
            return $ if all isTrivial redrules
                       then []
                       else [CmtFunc cmt qn ar vis texp redrules]
    else return [fdecl]

-- Translate property theorem into simplification rules.
theoremToSimpRules :: Rule QName -> [Rule QName]
theoremToSimpRules (_, TermVar _) =
  error $ "theoremToSimpRules: variable in rhs"
theoremToSimpRules rl@(_, TermCons qf args)
  | qf == easyCheck "-=-" || qf == easyCheck "<~>"
  = [(TermCons (pre "==") args, trueTerm),
     (TermCons (pre "==") (reverse args), trueTerm)]
  | qf == easyCheck "always" = [(head args, trueTerm)]
  | otherwise = [rl]
 where
  easyCheck f = ("Test.EasyCheck",f)

isTrivial :: CRule -> Bool
isTrivial (CRule _ rhs) = case rhs of
  CSimpleRhs exp [] -> exp == constF (pre "True")
  _                 -> False

-- To avoid infinite loops during simplification, we define a maximum number
-- of allowed simplification steps:
maxSimpSteps :: Int
maxSimpSteps = 100

-- Simplify a rule of a postcondition.
simplifyRule :: Int -> TRS QName ->  QName -> CRule ->  IO CRule
simplifyRule verb simprules qn crule@(CRule rpats _) = do
  (id $!! (lhs,rhs)) `seq` done -- in order to raise a fromRule error here!
  unless (null trs) $
    error $ "simplifyRule: cannot handle local TRS:\n" ++ showTRS snd trs
  when (verb > 1 ) $ putStrLn $ unlines
    ["POSTCONDITION: " ++ showRule snd (lhs,rhs),
     "POSTCONDEXP:   " ++ showTerm snd postcondexp,
     "SIMPLIFIEDEXP: " ++ showTerm snd simpterm,
     "SIMPPOSTCOND:  " ++ showRule snd simppostcond ]     
  return (simpleRule rpats (term2acy (concatMap varsOfPat rpats) simppostrhs))
 where
   ((lhs,rhs),trs) = fromRule qn crule
   postcondexp     = postCondition2Term lhs rhs
   simpterm        = simplifyTerm maxSimpSteps simprules postcondexp
   simppostrhs     = postConditionTermToRule lhs simpterm
   simppostcond    = (lhs, simppostrhs)

--- Transform a post-condition rule into a term by substituting
---  the last argument variable by the function call.
postCondition2Term :: Term QName -> Term QName -> Term QName
postCondition2Term (TermVar _) _ =
  error "postCondition2Term: variable term"
postCondition2Term (TermCons (mn,fn) args) rhs =
  let TermVar i  = last args
      fcall      = TermCons (mn, fromPostCondName fn)
                            (take (length args - 1) args)
      fcallsubst = extendSubst emptySubst i fcall
   in applySubst fcallsubst rhs

--- Transform (simplified) post-condition back into rule by replacing
--- function call by the last argument variable. by the function call.
postConditionTermToRule :: Term QName -> Term QName -> Term QName
postConditionTermToRule (TermVar _) _ =
  error "postConditionTermToRule: variable term"
postConditionTermToRule (TermCons (mn,fn) args) term =
  let TermVar i  = last args
      fcall      = TermCons (mn, fromPostCondName fn)
                            (take (length args - 1) args)
   in replaceAllTerms (fcall, TermVar i) term

replaceAllTerms :: Rule QName -> Term QName -> Term QName
replaceAllTerms (lhs,rhs) term =
  if null oneStep
   then term
   else replaceAllTerms (lhs,rhs) (head oneStep)
 where
  oneStep = [ replaceTerm term p rhs | p <- positions term, (term |> p) == lhs ]

------------------------------------------------------------------------

simplifyTerm :: Int -> TRS QName -> Term QName -> Term QName
simplifyTerm maxsteps simprules term =
  if null oneStep || maxsteps==0
   then term
   else simplifyTerm (maxsteps-1) simprules (head oneStep)
 where
  oneStep = [ replaceTerm term p (applySubst sub rhs)
            | p <- positions term,
              rule <- simprules,
              let vMax = maximum (0: tVars term) + 1,
              let (lhs,rhs) = renameRuleVars vMax rule,
              sub <- maybeToList (match (term |> p) lhs) ]

-- match t1 t2 = sub  iff  sub(t2) = t1
match :: Term QName -> Term QName -> Maybe (Subst QName)
match = matchTerm emptySubst
 where
  matchTerm sub t1 (TermVar i) =
    maybe (Just (extendSubst sub i t1))
          (\t2 -> matchTerm sub t1 t2)
          (lookupSubst sub i)
  matchTerm sub (TermCons f1 args1) (TermCons f2 args2) =
    if f1 /= f2 then Nothing else matchArgs sub args1 args2
  matchTerm _ (TermVar _) (TermCons _ _) = Nothing

  matchArgs _ (_:_) [] = Nothing
  matchArgs _ [] (_:_) = Nothing
  matchArgs sub []  [] = Just sub
  matchArgs sub (x:xs) (y:ys) = maybe Nothing
                                      (\s -> matchArgs s xs ys)
                                      (matchTerm sub x y)


-- Some additional simplifcation rules (based on Prelude definitions):
standardSimpRules :: TRS QName
standardSimpRules =
  [ (TermCons (pre "&&") [trueTerm, x1], x1)
  , (TermCons (pre "&&") [x1, trueTerm], x1)
  ]
 where
  x1 = TermVar 1

trueTerm :: Term QName
trueTerm = TermCons (pre "True") []

------------------------------------------------------------------------
--- Translate terms into AbstractCurry expressions

-- to be extended
term2acy :: [CVarIName] -> Term QName -> CExpr
term2acy cvars (TermVar i) =
  maybe (error "term2acy: cannot find variable")
        (\s -> CVar (i,s))
        (lookup i cvars)
term2acy cvars (TermCons (mn,fn) args)
 | null args && head mn == '%' = CLit (const2literal (tail mn, fn))
 | otherwise
 = foldl CApply (CSymbol (mn,fn)) (map (term2acy cvars) args)

const2literal :: QName -> CLiteral
const2literal sl = case sl of
  ("i",s) -> CIntc   (readQTerm s)
  ("f",s) -> CFloatc (readQTerm s)
  ("c",s) -> CCharc  (head s)
  ("s",s) -> CStringc s
  _   -> error "const2literal: unknown literal"

------------------------------------------------------------------------
