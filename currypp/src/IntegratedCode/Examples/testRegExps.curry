{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some tests for integrated code to support
--- regular expression matching.
--- The syntax of regular expressions is similar to
--- POSIX extended regular expressions
------------------------------------------------------------------------------

import RegExp -- required in the pre-processed program
import Test.EasyCheck

test_abc = (match ``regex abc'' "abc")  -=- True

test_abastarc = (match ````regex aba*c'''' "abaaaaaaaaaaaaac")  -=- True

test_a_bcstar_plus = (match ``regex (a|(bc*))+'' "aabcccaba")   -=- True

test_alpha_1 = (match ``regex [:alpha:]'' "a")                  -=- True

test_alpha_2 = (not (match ``regex [:alpha:]'' "4"))            -=- True

test_alpha_star_1 = (match ``regex [:alpha:]*'' "Abc")          -=- True

test_alpha_star_2 = (match ``regex [:alpha:]*'' "ab9c")         -=- False

test_a_z_plus_1 = (match ``regex [a-z]+'' "abc")                -=- True

test_a_z_plus_2 = (match ``regex [a-z]+'' "Abc")                -=- False

-- Examples with parameterized regular expressions:

pregexp1 :: Ord a => a -> a -> [a] -> Bool
pregexp1 v1 v2 = match ``regex [<v1>-<v2>]*''

test_para_a_c_1 = pregexp1 'a' 'c' "abccba"               -=- True

test_para_a_c_2 = pregexp1 'a' 'c' "abcdcba"              -=- False

pregexp2 :: Ord a => a -> a -> [a] -> Bool
pregexp2 v1 v2 = match ``regex (<v1>|<v2>)*''

test_para_0_1_star_1 = pregexp2 0 1 [0,1,1,0,0]           -=- True

test_para_0_1_star_2 = pregexp2 0 1 [0,1,2,0,0]           -=- False

-- A regular expression containing a complex Curry expression:
test_complexexp = (match ``regex <((\x -\> x) 'a')>'' "a") -=- True


-- Email address matching:
isEmail :: String -> Bool
isEmail = match ``regex
  [a-zA-Z0-9]([a-zA-Z0-9\._])*
  @
  [a-zA-Z0-9][a-zA-Z0-9\-]*\.
  ([:alnum:][a-zA-Z0-9\-]*\.)*
  [a-zA-Z]{2,4}''

test_Email1 = isEmail "pakcs@curry-language.org"      -=- True

test_Email2 = isEmail "pa%kcs@curry-language.org"     -=- False


isID :: String -> Bool
isID = match ``regex [a-zA-Z][a-zA-Z0-9_']*''

test_ID1 = isID "ab_4'aux"    -=- True

test_ID2 = isID "4ab"         -=- False

