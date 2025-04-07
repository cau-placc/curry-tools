module Tests where

import Data.Trie as T
import Test.Prop
import Data.Maybe       ( isNothing )
import Data.List        ( sort, nub, intersect )
import Data.Tuple.Extra ( second )

--- Property: an empty trie has size 0.
testEmptySize :: Prop
testEmptySize = T.null T.empty -=- True

--- Property: a singleton trie has size 1.
testSingletonSize :: Prop
testSingletonSize = size (singleton "a" 42) -=- 1

--- Property: the size of a trie is equal to the number of elements in it,
---           where the keys are unique.
testSize :: [(String, Int)] -> Prop
testSize input = uniqueKeys input 
             ==> size (fromList input) -=- length input

--- Property: the size of a trie is equal to the number of elements in it,
---           where the keys of the input list are not necessarily unique.
testSize2 :: [(String, Int)] -> Prop
testSize2 input = size (fromList input') -=- length (nub $ map fst input')
 where 
  input' = input ++ input

--- Property: all elements in a trie can be looked up.
testKeys :: [(String, Int)] -> Prop
testKeys c = forAll keys $ \k -> trie `containsKey` k -=- True
 where
  keys = map fst c
  trie = fromList c

--- Property: trying to look up a non-existing key in a trie is unsuccessful.
testExtranousKeys :: [(String, Int)] -> Prop
testExtranousKeys input = forAll extra $ \k -> trie `containsKey` k -=- False
 where
  keys  = map fst input
  trie  = fromList input
  extra = take (length input) $ freshKeys keys

--- Property: removing one key from a non-empty trie results 
---           in a trie with one less element, namely, 
---           the one associated with the removed key.
testDeleteOne :: [(String, Int)] -> Prop
testDeleteOne input = not (Prelude.null input) 
                   ==> let nt = delete k trie 
                       in size nt == (size trie - 1) 
                        && isNothing (T.lookup k nt) -=- True
 where
  keys = map fst input
  trie = fromList input
  k    = head keys

--- Property: removing all keys from a trie results in an empty trie.
testDelete :: [(String, Int)] -> Prop
testDelete input = (foldr delete trie keys) -=- T.empty
 where
  keys = nub $ map fst input
  trie = fromList input

--- Property: removing a non-existing key from a trie does not change the trie.
testDeleteNonExisting :: [(String, Int)] -> Prop
testDeleteNonExisting input = forAll extra $ \k -> delete k trie -=- trie
 where
  keys  = nub $ map fst input
  trie  = fromList input
  extra = take (length input) $ freshKeys keys

--- Property: `toList . fromList` behaves like the identity function 
--- (ignoring potential loss of order). That is, all information is preserved.
testConversion :: [(String, Int)] -> Prop
testConversion input = uniqueKeys content 
                   ==> (sort . toList . fromList) content -=- sort content
 where
  content = input ++ [("a", 42), ("b", 43), ("ab", 44)]

--- Tests functor instance of Data.Trie.
testFmap :: [(String, Int)] -> Prop
testFmap input = uniqueKeys input
            ==> (sort . toList . fmap (+1) . fromList) input 
                 -=- (sort . map (second (+ 1))) input

--- Test: Updating a key in a trie with a new value 
--- results in the new value being associated with the key.
testUpdate :: [(String, Int)] -> Prop
testUpdate input = 
  fmap (+1) trie -=- foldr (\k t -> update k (\(Just v) -> v + 1) t) trie keys
 where
  trie = T.fromList input
  keys = nub $ map fst input 

-------------------------------------------------------------------------------
--- Auxiliary functions

--- Checks whether the keys in the input list are unique.
uniqueKeys :: [(String, a)] -> Bool
uniqueKeys xs = length xs == (length . nub . map fst) xs

--- Checks whether two lists are disjoint.
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint = (Prelude.null .) . intersect

--- Generates an infinite list of fresh keys.
freshKeys :: [String] -> [String]
freshKeys keys = filter (`notElem` keys) (map show [1..])