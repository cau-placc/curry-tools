{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

import Test.EasyCheck

-- Reverse a list if it has exactly two elements:
rev2 :: [a] -> [a]
rev2 [x,y] = [y,x]
rev2'default xs = xs

main :: [[Int]]
main = map rev2 (map (\n->[1..n]) [0..4])

test1 = rev2 [] -=- ([] :: [Int])
test2 = rev2 [1] -=- [1]
test3 = rev2 [1,2] -=- [2,1]
test4 = rev2 [1,2,3] -=- [1,2,3]
test5 = rev2 [1,2,3,4] -=- [1,2,3,4]

test6 xs = length xs /= 2 ==> rev2 xs -=- xs
