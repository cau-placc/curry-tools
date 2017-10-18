-- Half of a natural number:

import Test.EasyCheck


data Peano = O | S Peano
  deriving (Eq,Show)

toPeano :: Int -> Peano
toPeano n = if n==0 then O else S (toPeano (n-1))

fromPeano :: Peano -> Int
fromPeano O = 0
fromPeano (S p) = 1 + fromPeano p

add :: Peano -> Peano -> Peano
add O     p = p
add (S p) q = S (add p q)

half :: Peano -> Peano
half y | add x x == y
       = x
 where x free 

main :: Int
main = fromPeano (half (toPeano 100))

test_half = fromPeano (half (toPeano 100)) -=- 50
