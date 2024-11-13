-- Tests for the non-determinism dependency analysis `NonDetDeps`.
--
-- Runt test with:
-- > cass NonDetDeps NonDetTest.curry

last :: [Int] -> Int
last xs | _ ++ [x] == xs = x where x free

lastfp :: [Int] -> Int
lastfp (_ ++ [x]) = x

printLast :: IO ()
printLast = do
  print $ last [1..7]
  print $ lastfp [1..42]

coin :: Int
coin = 0 ? 1

lastCoin :: Int
lastCoin = id (last [coin])
--> last, coin

f :: Int -> Int
f x = x ? lastCoin

g :: Int -> Int
g x = f x
-- For this operation, the NonDetDeps analysis reports that the
-- non-determinism depends on `f`.
-- However, the analysis NonDetAllDeps reports also the dependency
-- on the non-deterministic operations coin, last,...

