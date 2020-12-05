-----------------------------------------------------------------------------
--- An efficient implementation of set based on finite maps.
-----------------------------------------------------------------------------

module Data.Set
  ( Set, null, size, fromList, empty, insert, member, delete
  , union, toList, difference
  ) where

import qualified Data.Map as Map

-------------------------------------------------------------------------
--                                                                      -
--   FiniteSets --- a thin veneer                                       -
--                                                                      -
-------------------------------------------------------------------------

--- The type of sets of elements.
type Set key = Map.Map key ()

--- Returns an empty set.
empty :: Set key
empty = Map.empty

--- Transforms a list into a set of its elements.
fromList :: Ord key => [key] -> Set key
fromList xs = Map.fromList [ (x, ()) | x <- xs]

--- Test for an empty set.
null :: Set key -> Bool
null = Map.null

--- Inserts an element into a set if it is not already there.
insert :: Ord key => key -> Set key -> Set key
insert k s = Map.insert k () s

--- Deletes an element from a set.
delete :: Ord key => key -> Set key -> Set key
delete k s = Map.delete k s

--- Computes the size of two sets.
size :: Set key -> Int
size = Map.size

--- Returns `True` if an element is contained in a set.
--- @param e - an element to be checked for containment
--- @param s - a set
--- @return `True` if `e` is contained in `s`
member :: Ord key => key -> Set key -> Bool
member = Map.member

--- Computes the difference of two sets.
difference :: Ord key => Set key -> Set key -> Set key
difference = Map.difference

--- Transforms a set into an ordered list of its elements.
toList :: Set key -> [key]
toList = Map.keys

--- Computes the union of two sets.
union :: Ord key => Set key -> Set key -> Set key
union = Map.union
