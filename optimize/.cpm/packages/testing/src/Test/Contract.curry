------------------------------------------------------------------------
--- This library defines some auxiliaries to check contracts
--- based on specifications or pre- and postconditions provided
--- in a Curry module.
--- The interface might probably change with the further
--- development of the contract implementation.
---
--- @author Michael Hanus
--- @version May 2016
--- @category general
------------------------------------------------------------------------

module Test.Contract
  ( withContract1, withContract2
  , withContract1ND, withContract2ND
  , withPreContract1, withPreContract2
  , withPostContract0, withPostContract1, withPostContract2
  , withPostContract0ND, withPostContract1ND, withPostContract2ND
  )  where

import SetFunctions
import System.IO.Unsafe(trace)

---------------------------------------------------------------------------
-- Report the result of checking the pre/postconditions.
-- The first argument is a tag (the name of the operation).
-- The second argument is unified with () (used by enforceable constraints).
-- The third argument is a Boolean result that must not be False for
-- a satisfied condition.
-- The fourth argument is a string describing the context (arguments,result).

checkPre :: String -> Bool -> String -> Bool
checkPre fname result arg = case result of
  True  -> True
  False -> traceLines
             ["Precondition of "++fname++" violated for:",arg]
             (error "Execution aborted due to contract violation!")

checkPreND :: String -> Values Bool -> String -> Bool
checkPreND fname result arg = case False `valueOf` result of
  True  -> traceLines
             ["Precondition of "++fname++" violated for:",arg]
             (error "Execution aborted due to contract violation!")
  False -> True

checkPost :: String -> Bool -> String -> Bool
checkPost fname result arg = case result of
  True  -> True
  False -> traceLines
             ["Postcondition of "++fname++" violated "++
              "for:", arg]
             (error "Execution aborted due to contract violation!")

checkPostND :: String -> Values Bool -> String -> Bool
checkPostND fname result arg = case False `valueOf` result of
  True  -> traceLines
             ["Postcondition of "++fname++" violated "++
              "for:", arg]
             (error "Execution aborted due to contract violation!")
  False -> True

-- print some lines of output on stderr with flushing after each line
traceLines :: [String] -> a -> a
traceLines ls x = trace (unlines ls) x

-- show operation used to show argument or result terms to the user:
-- Currently, we use Prelude.show but this has the risk that it suspends
-- or loops.
showATerm :: Show a => a -> String
showATerm = show -- or Unsafe.showAnyTerm                 --

---------------------------------------------------------------------------
-- Combinators for checking of contracts having pre- and postconditions

withContract1 :: (Show a, Show b) => String
              -> (a -> Bool) -> (a -> b -> Bool) -> (b -> b)
              -> (a -> b) -> a -> b
withContract1 fname precond postcond postobserve fun arg
  |  checkPre fname (precond arg) (showATerm arg)
  &> checkPost fname (postcond arg result)
               (unwords [showATerm arg, "->", showATerm (postobserve result)])
  = result
 where result = fun arg

withContract1ND :: (Show a, Show b) => String
                -> (a -> Values Bool) -> (a -> b -> Values Bool) -> (b -> b)
                -> (a -> b) -> a -> b
withContract1ND fname precond postcond postobserve fun arg
  |  checkPreND fname (precond arg) (showATerm arg)
  &> checkPostND fname (postcond arg result)
               (unwords [showATerm arg, "->", showATerm (postobserve result)])
  = result
 where result = fun arg

withContract2 :: (Show a, Show b, Show c) => String
              -> (a -> b -> Bool) -> (a -> b -> c -> Bool)
              -> (c -> c) -> (a -> b -> c) -> a -> b -> c
withContract2 fname precond postcond postobserve fun arg1 arg2
  |  checkPre fname (precond arg1 arg2)
                       (unwords [showATerm arg1,showATerm arg2])
  &> checkPost fname (postcond arg1 arg2 result)
               (unwords [showATerm arg1, showATerm arg2, "->",
                         showATerm (postobserve result)])
  = result
 where result = fun arg1 arg2

withContract2ND :: (Show a, Show b, Show c) => String
                -> (a -> b -> Values Bool) -> (a -> b -> c -> Values Bool)
                -> (c -> c) -> (a -> b -> c) -> a -> b -> c
withContract2ND fname precond postcond postobserve fun arg1 arg2
  |  checkPreND fname (precond arg1 arg2)
                      (unwords [showATerm arg1,showATerm arg2])
  &> checkPostND fname (postcond arg1 arg2 result)
                 (unwords [showATerm arg1, showATerm arg2, "->",
                           showATerm (postobserve result)])
  = result
 where result = fun arg1 arg2

---------------------------------------------------------------------------
-- Combinators for checking contracts without postconditions:

withPreContract1 :: Show a => String -> (a -> Bool) -> (a -> b) -> a -> b
withPreContract1 fname precond fun arg
  | checkPre fname (precond arg) (showATerm arg)
  = fun arg

withPreContract2 :: (Show a, Show b) => String -> (a -> b -> Bool)
                 -> (a -> b -> c) -> a -> b -> c
withPreContract2 fname precond fun arg1 arg2
  | checkPre fname (precond arg1 arg2)
                      (unwords [showATerm arg1,showATerm arg2])
  = fun arg1 arg2

---------------------------------------------------------------------------
-- Combinators for checking contracts without preconditions:

-- Add postcondition contract to 0-ary operation:
withPostContract0 :: Show a => String -> (a -> Bool) -> (a -> a) -> a -> a
withPostContract0 fname postcond postobserve val
  | checkPost fname (postcond val) (unwords [showATerm (postobserve val)])
  = val

-- Add postcondition contract to 0-ary operation:
withPostContract0ND :: Show a => String -> (a -> Values Bool) -> (a -> a) -> a -> a
withPostContract0ND fname postcond postobserve val
  | checkPostND fname (postcond val) (unwords [showATerm (postobserve val)])
  = val

withPostContract1 :: (Show a, Show b) => String -> (a -> b -> Bool) -> (b -> b)
                  -> (a -> b) -> a -> b
withPostContract1 fname postcond postobserve fun arg
  | checkPost fname (postcond arg result)
              (unwords [showATerm arg, "->", showATerm (postobserve result)])
  = result
 where result = fun arg

withPostContract1ND :: (Show a, Show b) => String
                    -> (a -> b -> Values Bool) -> (b -> b)
                    -> (a -> b) -> a -> b
withPostContract1ND fname postcond postobserve fun arg
  | checkPostND fname (postcond arg result)
                (unwords [showATerm arg, "->", showATerm (postobserve result)])
  = result
 where result = fun arg

withPostContract2 :: (Show a, Show b, Show c) =>
                     String -> (a -> b -> c -> Bool) -> (c -> c)
                  -> (a -> b -> c) -> a -> b -> c
withPostContract2 fname postcond postobserve fun arg1 arg2
  | checkPost fname (postcond arg1 arg2 result)
              (unwords [showATerm arg1, showATerm arg2, "->",
                        showATerm (postobserve result)])
  = result
 where result = fun arg1 arg2

withPostContract2ND :: (Show a, Show b, Show c) =>
                       String -> (a -> b -> c -> Values Bool) -> (c -> c)
                    -> (a -> b -> c) -> a -> b -> c
withPostContract2ND fname postcond postobserve fun arg1 arg2
  | checkPostND fname (postcond arg1 arg2 result)
                (unwords [showATerm arg1, showATerm arg2, "->",
                          showATerm (postobserve result)])
  = result
 where result = fun arg1 arg2

---------------------------------------------------------------------------
