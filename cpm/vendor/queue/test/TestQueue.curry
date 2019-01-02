import List

import Test.Prop
import System.Random

import Data.Queue

deq f = f . listToDeq

deqs f = deqToList . f . listToDeq

testHead = eq (deq deqHead) head

testLast = eq (deq deqLast) last

testCons = eq (deqs (cons 73)) (73:)

testTail = eq (deqs (deqTail)) tail

testSnoc = eq (deqs (snoc 73)) (++[73])

testInit = eq (deqs deqInit) init
 where
  init [x] = []
  init (x:y:ys) = x : init (y:ys)

testReverse = eq (deqs deqReverse) reverse

testLength  = eq (deq deqLength) length

testRotate  = eq (deqs rotate) (\ (x:xs) -> xs ++ [x])


------------------------------------------------------------------------------
-- Random test:

--- Tests a given predicate on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
test :: ([Int] -> Bool) -> PropIO
test f =
  (rndList lenRnds >>= \xs -> return (if f xs then Nothing else Just xs))
  `returns` Nothing

--- Tests whether two operations return equal results
--- on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
eq :: Eq a => ([Int] -> a) -> ([Int] -> a) -> PropIO
eq f g = test (\x -> (f x)==(g x))

--- generate a list of at most n random numbers (without duplicated elements)
rndList :: Int -> IO [Int]
rndList n = getRandomSeed >>= return . nub . take n . (flip nextIntRange 100000)

--- maximal length of test lists
lenRnds :: Int
lenRnds = 1000

------------------------------------------------------------------------------
