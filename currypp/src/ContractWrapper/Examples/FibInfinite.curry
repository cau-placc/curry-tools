{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=contracts #-}

import Test.Prop

-- An infinite list of Fibonacci numbers specified by traditional
-- recursive definition

-- (Deterministic!) specification of all Fibonacci numbers:
fibs'spec :: [Int]
fibs'spec = map fib [0..]
 where fib n | n == 0 = 0
             | n == 1 = 1
             | otherwise = fib (n-1) + fib (n-2)

-- In order to check the infinite result list, we define a specific observer:
fibs'post'observe xs = take 10 xs

-- A more efficient (but erroneous) implementation of all Fibonacci numbers:
fibs :: [Int]
fibs = fiblist 0 1
 where
  fiblist x y = x : fiblist (x+y) y

main = take 4 fibs

fibsViolation = toError (take 4 fibs)
