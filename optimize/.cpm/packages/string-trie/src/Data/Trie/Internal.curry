------------------------------------------------------------------------------
--- Internal representation of a trie.
---
--- @author Lasse Züngel
--- @version November 2024
------------------------------------------------------------------------------

module Data.Trie.Internal where 

import Data.Maybe ( isNothing )

--- Internal representation of a trie.
data InternalTrie a = InternalTrie (Maybe a) [(Char, InternalTrie a)]
  deriving (Show, Eq)

--- An empty internal trie.
empty' :: InternalTrie a
empty' = InternalTrie Nothing []

--- Returns true iff the internal trie is empty.
null' :: InternalTrie a -> Bool
null' t = case t of 
  InternalTrie Nothing [] -> True
  _                       -> False

--- A singleton internal trie.
singleton' :: String -> a -> InternalTrie a
singleton' str v =
  foldr (\c t -> InternalTrie Nothing [(c, t)]) (InternalTrie (Just v) []) str

-- Updates or inserts a value in the internal trie and
-- returns whether the size has increased.
update' :: String -> (Maybe a -> a) -> InternalTrie a -> (Bool, InternalTrie a)
update' []     f (InternalTrie old ts) = (isNothing old,  InternalTrie (Just $ f old) ts)
update' (c:cs) f (InternalTrie v' ts) = case Prelude.lookup c ts of
  Nothing -> let t' = singleton' cs (f Nothing)
             in (True, InternalTrie v' ((c, t') : ts))
  Just t  -> let (incr, t') = update' cs f t
             in (incr, InternalTrie v' ((c, t') : (filter (\(c', _) -> c' /= c) ts)))

-- Tries to delete a key from the internal trie. If the key is not in the trie,
-- `Nothing` is returned. This feedback is used to determine whether the size of the trie
-- has changed.
delete' :: String -> InternalTrie a -> Maybe (InternalTrie a)
delete' k (InternalTrie v ts) = do
  res <- case k of
    []     -> v *> Just (InternalTrie Nothing ts)
    (c:cs) -> do 
      t  <- Prelude.lookup c ts
      t' <- delete' cs t
      let e  = (c, t') 
          es = filter ((/= c) . fst) ts
      return $ InternalTrie v (if null' t then es else e : es)
  return $ sanitize res
 where
  sanitize :: InternalTrie a -> InternalTrie a
  sanitize (InternalTrie v' ts') =
    InternalTrie v' (filter (not . null' . snd) ts')

--- Looks up a value in the internal trie.
lookup' :: String -> InternalTrie a -> Maybe a
lookup' []     (InternalTrie v _)  = v
lookup' (c:cs) (InternalTrie _ ts) = Prelude.lookup c ts >>= lookup' cs

--- Converts an internal trie into a list of key-value pairs.
toList' :: InternalTrie a -> [(String, a)]
toList' (InternalTrie v ts) = case v of
  Nothing -> concatMap (\(c, t) -> map (\(s, w) -> (c:s, w)) (toList' t)) ts
  Just z  -> ("", z) :
              concatMap (\(c, t) -> map (\(s, w) -> (c:s, w)) (toList' t)) ts

instance Functor InternalTrie where
  fmap f (InternalTrie v ts) = InternalTrie (fmap f v) (map (second (fmap f)) ts)

-------------------------------------------------------------------------------
--- Auxiliary functions

second :: (a -> b) -> (c, a) -> (c, b)
second f (x, y) = (x, f y)