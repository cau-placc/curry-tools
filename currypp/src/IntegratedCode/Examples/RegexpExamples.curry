{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for integrated code to support
--- regular expression matching.
--- The syntax of regular expressions is similar to
--- POSIX extended regular expressions
------------------------------------------------------------------------------

import RegExp -- required in the pre-processed program

check1 :: Bool
check1 = match ``regex abc'' "abc"

check2 :: Bool
check2 = match ``regex aba*c'' "abaaaaaaaaaaaaac"

check3 :: String -> Bool
check3 = match ``regex (a|(bc*))+''

check4 :: String -> Bool
check4 = match ``regex [:alpha:]''

check5 :: String -> Bool
check5 = match ``regex [a-z]+''

-- Examples with parameterized regular expressions:

pregexp1 :: Ord a => a -> a -> [a] -> Bool
pregexp1 v1 v2 = match ``regex [<v1>-<v2>]*''

pregexp2 :: Ord a => a -> a -> [a] -> Bool
pregexp2 v1 v2 = match ``regex (<v1>|<v2>)*''

-- A regular expression containing a complex Curry expression:
check6 :: Bool
check6 = match ``regex <((\x -\> x) 'a')>'' "a"

