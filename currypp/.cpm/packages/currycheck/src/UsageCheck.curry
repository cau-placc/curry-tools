---------------------------------------------------------------------------
--- Set functions are intended to exist for every top-level function.
--- The operation `checkSetUse` detects unintended uses of set funtions.
--- Furthermore, the operation `checkBlacklistUse` checks whether
--- internal operations like `Prelude.=:<=` or `Prelude.prim_` are used
--- in a Curry program.
---
--- See example program `Examples/UsageErrors.curry` for some examples.
---
--- @author Michael Hanus
--- @version October 2016
---------------------------------------------------------------------------

module UsageCheck(checkSetUse, checkBlacklistUse) where

import qualified AbstractCurry.Types as AC
import AbstractCurryMatch
import Char(isDigit)
import FlatCurry.Types
import FlatCurryMatch
import Read(readNat)
import SetFunctions

---------------------------------------------------------------------
--- Returns messages about unintended uses of set functions in a
--- FlatCurry program.
checkSetUse :: Prog -> IO [(QName,String)]
checkSetUse (Prog _ _ _ fdecls _) = do
  seterrors <- values2list (set1 setUse fdecls)
  return (map showSetError seterrors)
 where
  showSetError (qf,sar) =
    (qf, "wrong use of set function `set" ++ sar ++ "'!")

--- Returns some unintended use of a set function occurring in a list
--- of function declarations. The name of the function together with
--- the arity of the set function used is returned.
--- Set functions are intended to be used only on top-level functions
--- with the right arity.
---
--- To provide a simple implementation, we exploit functional patterns
--- with the function `funWithinExp`.
setUse :: [FuncDecl] -> (QName, String)
--setUse (_ ++ [funWithExp qf (Comb ct ("SetFunctions","set"++n) args)] ++ _)
setUse (_++ [funWithinExp qf _ _ (Comb ct ("SetFunctions","set"++n) args)] ++_)
  | not (validSetFunCall ct n args) = (qf,n)

--- Checks whether an application of a set function is as intended.
validSetFunCall :: CombType -> String -> [Expr] -> Bool
validSetFunCall ct n args
  | ct==FuncCall && all isDigit n && not (null args)
  = if arity==0 then isFuncCall (head args)
                else isFuncPartCall arity (head args)
 where
  arity = readNat n

isFuncCall :: Expr -> Bool
isFuncCall e = case e of
  Comb FuncCall qf [] -> isID qf
  _                   -> False

isFuncPartCall :: Int -> Expr -> Bool
isFuncPartCall n e = case e of
  Comb (FuncPartCall p) qf _ -> p==n && isID qf
  _                          -> False

isID :: QName -> Bool
isID (_,n) = all (`elem` infixIDs) n || '.' `notElem` n
 where
  infixIDs :: String
  infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

---------------------------------------------------------------------
--- Returns messages about uses of black-listed operations occurring
--- in an AbstractCurry program.
checkBlacklistUse :: AC.CurryProg -> IO [(QName,String)]
checkBlacklistUse (AC.CurryProg _ _ _ _ _ _ cfdecls _) = do
  blerrors <- values2list (set1 blacklistUsage cfdecls)
  return (map showBlacklistError blerrors)
 where
  showBlacklistError (qf,(q,f)) =
    (qf, "direct use of `" ++ q++"."++f ++ "' not allowed!")

--- Returns some use of a black-listed operation occurring in a list
--- of function declarations. The name of the defined function together with
--- the black-listed operation is returned.
---
--- To provide a simple implementation, we exploit functional patterns
--- with the function `cfunWithExp`.
---
--- TODO: check also occurrences in functional patterns
blacklistUsage :: [AC.CFuncDecl] -> (AC.QName, AC.QName)
blacklistUsage (_ ++ [cfunWithExp qf (AC.CSymbol qop)] ++ _)
  | isBlacklistedOperation qop
  = (qf,qop)

isBlacklistedOperation :: AC.QName -> Bool
isBlacklistedOperation (q,f) =
  q == AC.preludeName &&
  (take 5 f == "prim_" --no direct call to primitive ops
   || f `elem` ["=:<=", "=:<<="])

---------------------------------------------------------------------
