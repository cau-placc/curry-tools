--- This module contains some specifications of operations from the List
--- module.
--- Since the preprocessor also depends on the list module,
--- we cannot write these specifications directly into the list module.

{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

import qualified List
import Test.Prop

nub :: Eq a => [a] -> [a]
nub = List.nub

nub'spec :: Eq a => [a] ->DET [a]
nub'spec (xs++[e]++ys++[e]++zs) = nub'spec (xs++[e]++ys++zs)
nub'spec'default xs = xs

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf = List.isPrefixOf

isPrefixOf'spec :: Eq a => [a] -> [a] ->DET Bool
isPrefixOf'spec xs (xs ++ _) = True
isPrefixOf'spec'default _ _  = False


isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf = List.isSuffixOf

isSuffixOf'spec :: Eq a => [a] -> [a] ->DET Bool
isSuffixOf'spec xs (_ ++ xs) = True
isSuffixOf'spec'default _ _  = False


isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf = List.isInfixOf

isInfixOf'spec :: Eq a => [a] -> [a] ->DET Bool
isInfixOf'spec xs (_ ++ xs ++ _) = True
isInfixOf'spec'default _ _       = False

