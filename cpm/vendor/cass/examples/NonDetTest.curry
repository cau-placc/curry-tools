-- Tests for the non-determinism dependency analysis `NonDetDeps`.
--
-- Runt test with:
-- > cass NonDetDeps NonDetTest.curry

import SetFunctions

last xs | _ ++ [x] == xs = x where x free

lastfp (_ ++ [x]) = x

printLast = do
  print $ last [1..7]
  print $ lastfp [1..42]

coin = 0 ? 1

lastCoin = id (last [coin])
--> last, coin


f x = x ? lastCoin

g x = f x
-- For this operation, the NonDetDeps analysis reports that the
-- non-determinism depends on `f`.
-- However, the analysis NonDetAllDeps reports also the dependency
-- on the non-deterministic operations coin, last,...

main0 = set0 lastCoin
main1 = set1 last [1,2,3]
main2 = set1 last [1,2,coin]
