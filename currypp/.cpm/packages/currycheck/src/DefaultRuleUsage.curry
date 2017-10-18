------------------------------------------------------------------------
--- This module contains some operations to check and access
--- default rules in a Curry program.
---
--- @author Michael Hanus
--- @version October 2016
------------------------------------------------------------------------

module DefaultRuleUsage
  ( containsDefaultRules, checkDefaultRules
  , isDefaultFunc, isDefaultName, fromDefaultName
  )  where

import AbstractCurry.Types
import AbstractCurry.Select
import List

--- Does a program contains default rules?
containsDefaultRules :: CurryProg -> Bool
containsDefaultRules = not . null . filter isDefaultFunc . functions

--- Check correct usage of default rules and return function names and errors
--- for incorrect uses.
checkDefaultRules :: CurryProg -> [(QName,String)]
checkDefaultRules prog =
  let (defruledecls,fdecls) = partition isDefaultFunc (functions prog)
   in concatMap (checkDefaultRule fdecls) defruledecls

checkDefaultRule :: [CFuncDecl] -> CFuncDecl -> [(QName,String)]
checkDefaultRule funcs (CFunc defqn@(mn,deffn) ar _ _ rules)
  | null rules
  = [(defqn,"Default rule without right-hand side!")]
  | length rules > 1
  = [(defqn,"More than one default rule for function!")]
  | otherwise
  = maybe [(defqn,"Default rule given but no such function defined!")]
          (\fd -> if funcArity fd == ar
                  then []
                  else [(defqn,"Default rule has wrong arity!")])
          (find (\fd -> funcName fd == qn) funcs)
 where qn = (mn, fromDefaultName deffn)
checkDefaultRule funcs (CmtFunc _ qf ar vis texp rules) =
  checkDefaultRule funcs (CFunc qf ar vis texp rules)

--- Is this function a declaration of a default rule?
isDefaultFunc :: CFuncDecl -> Bool
isDefaultFunc = isDefaultName . snd . funcName

--- Is this the name of a specification?
isDefaultName :: String -> Bool
isDefaultName f = "'default" `isSuffixOf` f

--- Drop the default rule suffix "'default" from the name:
fromDefaultName :: String -> String
fromDefaultName f =
  let rf = reverse f
   in reverse (drop (if take 8 rf == "tluafed'" then 8 else 0) rf)

------------------------------------------------------------------------
