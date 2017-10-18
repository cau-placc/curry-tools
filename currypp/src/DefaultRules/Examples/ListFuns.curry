{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

import Test.EasyCheck

-- Warning: these redefinitions work fine with KiCS2, but with PAKCS
-- they do not work on infinite lists due to restrictions on set functions.

-- take with default rule:
take :: Int -> [a] -> [a]
take n (x:xs) | n>0 = x : take (n-1) xs
take'default _ _ = []

main1 = (take 0 [], take 2 [1..8], take 3 [1,2])

takeTest1 = take 0 []      -=-  ([] :: [Int])
takeTest2 = take 2 [1..8]  -=-  [1,2]
takeTest3 = take 3 [1,2]   -=-  [1,2]


-- zip3 with default rule (slight disadvantage: stricter than Prelude.zip3):
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs
zip3'default _ _ _ = []

main2 = zip3 [1,2,3] [4,5,6,7] [8,9]

zip3Test = zip3 [1,2,3] [4,5,6,7] [8,9]  -=-  [(1,4,8),(2,5,9)]
