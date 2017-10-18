------------------------------------------------------------------------
--- This module contains some operations to access contracts (i.e.,
--- specification and pre/postconditions) in a Curry program and
--- an operation to check the correct usage of specifications and
--- and pre/postconditions.
---
--- @author Michael Hanus
--- @version October 2016
------------------------------------------------------------------------

module ContractUsage
  ( checkContractUse
  , isSpecName, toSpecName, fromSpecName
  , isPreCondName, toPreCondName, fromPreCondName
  , isPostCondName, toPostCondName, fromPostCondName
  )  where

import AbstractCurry.Types
import AbstractCurry.Select
import AbstractCurry.Build  (boolType)
import List

--- Checks the intended usage of contracts.
checkContractUse :: CurryProg -> [(QName,String)]
checkContractUse prog =
  let mn           = progName prog
      allops       = map nameArityOfFunDecl (functions prog)
      specops      = map (\ (n,a) -> (fromSpecName n, a))
                         (funDeclsWithNameArity isSpecName prog)
      preops       = map (\ (n,a) -> (fromPreCondName n, a))
                         (funDeclsWithNameArity isPreCondName prog)
      postops      = map (\ (n,a) -> (fromPostCondName n, a-1))
                         (funDeclsWithNameArity isPostCondName prog)
      onlyprecond  = preops  \\ allops
      onlypostcond = postops \\ allops
      onlyspec     = specops \\ allops
      errmsg   = "No implementation (of right arity) for this "
      preerrs  = map (\ (n,_) -> ((mn, n++"'pre"), errmsg ++ "precondition"))
                     onlyprecond
      posterrs = map (\ (n,_) -> ((mn, n++"'post"), errmsg ++ "postcondition"))
                     onlypostcond
      specerrs = map (\ (n,_) -> ((mn, n++"'spec"), errmsg ++ "specification"))
                     onlyspec
   in preerrs ++ posterrs ++ specerrs ++ checkPrePostResultTypes prog

checkPrePostResultTypes :: CurryProg -> [(QName,String)]
checkPrePostResultTypes prog =
  let allops   = functions prog
      preops   = filter (isPreCondName  . snd . funcName) allops
      postops  = filter (isPostCondName . snd . funcName) allops
      errmsg c = c ++ " has illegal type"
      preerrs  = map (\fd -> (funcName fd, errmsg "Precondition"))
                     (filter (not . hasBoolResultType) preops)
      posterrs = map (\fd -> (funcName fd, errmsg "Postcondition"))
                     (filter (not . hasBoolResultType) postops)
   in preerrs ++ posterrs

hasBoolResultType :: CFuncDecl -> Bool
hasBoolResultType fd = resultType (typeOfQualType (funcType fd)) == boolType

-- Get function names from a Curry module with a name satisfying the predicate:
funDeclsWithNameArity :: (String -> Bool) -> CurryProg -> [(String,Int)]
funDeclsWithNameArity pred prog =
  map nameArityOfFunDecl
      (filter (pred . snd . funcName) (functions prog))

-- Computes the unqualified name and the arity from the type of the function.
nameArityOfFunDecl :: CFuncDecl -> (String,Int)
nameArityOfFunDecl fd =
  (snd (funcName fd), length (argTypes (typeOfQualType (funcType fd))))


-- Is this the name of a specification?
isSpecName :: String -> Bool
isSpecName f = "'spec" `isSuffixOf` f

--- Transform a name into a name of the corresponding specification
--- by adding the suffix "'spec".
toSpecName :: String -> String
toSpecName = (++"'spec")

-- Drop the specification suffix "'spec" from the name:
fromSpecName :: String -> String
fromSpecName f =
  let rf = reverse f
   in reverse (drop (if take 5 rf == "ceps'" then 5 else 0) rf)

-- Is this the name of a precondition?
isPreCondName :: String -> Bool
isPreCondName f = "'pre" `isSuffixOf` f

--- Transform a name into a name of the corresponding precondition
--- by adding the suffix "'pre".
toPreCondName :: String -> String
toPreCondName = (++"'pre")

-- Drop the precondition suffix "'pre" from the name:
fromPreCondName :: String -> String
fromPreCondName f =
  let rf = reverse f
   in reverse (drop (if take 4 rf == "erp'" then 4 else 0) rf)

-- Is this the name of a precondition?
isPostCondName :: String -> Bool
isPostCondName f = "'post" `isSuffixOf` f

--- Transform a name into a name of the corresponding prostcondition
--- by adding the suffix "'post".
toPostCondName :: String -> String
toPostCondName = (++"'post")

-- Drop the postcondition suffix "'post" from the name:
fromPostCondName :: String -> String
fromPostCondName f =
  let rf = reverse f
   in reverse (drop (if take 5 rf == "tsop'" then 5 else 0) rf)

------------------------------------------------------------------------
