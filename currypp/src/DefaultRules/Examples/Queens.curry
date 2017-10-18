{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

import Test.EasyCheck

-------------------------------------------------------------------------
-- Definition of n-queens with default rules:

-- Some permutation of a list of elements:
perm :: [a] -> [a]
perm [] = []
perm (x:xs) = ndinsert (perm xs)
 where
  ndinsert ys     = x : ys
  ndinsert (y:ys) = y : ndinsert ys

-- A placement is safe if two queens are not in a same diagonal:
safe (_++[x]++y++[z]++_) | abs (x-z) == length y + 1 = failed
safe'default xs = xs

-- A solution to the n-queens puzzle is a safe permutation:
queens :: Int -> [Int]
queens n = safe (perm [1..n])

test_queens4 = queens 4  <~>  ([3,1,4,2] ? [2,4,1,3])

test_queens6 = queens 6  <~>
               ([5,3,1,6,4,2] ? [4,1,5,2,6,3] ? [3,6,2,5,1,4] ? [2,4,6,1,3,5])

-------------------------------------------------------------------------
