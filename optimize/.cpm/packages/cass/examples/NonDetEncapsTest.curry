-- Tests for the non-determinism dependency analysis `NonDetDeps`
-- where search encapsulation operators are used.
--
-- Runt test with:
-- > cass NonDetDeps NonDetEncapsTest.curry

import Control.Search.SetFunctions
import Control.Search.Unsafe ( allValues )

coin :: Int
coin = 0 ? 1

allCoinsUnsafe :: [Int]
allCoinsUnsafe = allValues coin

allCoins :: [Int]
allCoins = sortValues (set0 coin)
