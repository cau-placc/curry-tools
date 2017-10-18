-- Various test functions

import Test.EasyCheck

f :: Eq a => [a] -> [a] -> a
f xs ys | xs == _++[x] && ys == _++[x]++_ = x   where x free

main :: Int
main = f [1,2] [2,1]

test_f = f [1,2] [2,1] -=- 2
