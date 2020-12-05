-- Some tests for sets (to be extended...)

import Prelude hiding ( null )
import Test.Prop

import Data.Set

testSizeFromList1 :: Int -> Prop
testSizeFromList1 m = size (fromList [1 .. n]) -=- n
 where n = toPos m

testSizeFromList2 :: Int -> Prop
testSizeFromList2 m =
  size (fromList [n, n-1 .. 1] `union` fromList [1 .. n]) -=- n
 where n = toPos m

testDeleteAll :: Int -> Prop
testDeleteAll m = always (null (foldr delete (fromList [1 .. n]) [1 .. n]))
 where n = toPos m

toPos :: Int -> Int
toPos n | n < 0 = 2 * abs n
        | otherwise = n
