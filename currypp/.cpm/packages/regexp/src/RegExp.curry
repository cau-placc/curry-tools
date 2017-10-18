------------------------------------------------------------------------------
--- A library to defines a structure of regular expressions and
--- a simple match operation for regular expressions.
--- This library is used to translate integrated code in the form
--- of POSIX extended regular expressions into Curry programs.
---
--- @author Jasper Sikorra
--- @version July 2017
--- @category general
------------------------------------------------------------------------------
-- TODO
-- - Stard and End are not implemented
-- - No function to match only a part of a lists

module RegExp(match, RegExp, ORegExp(..)) where

import List

--- Data type for regex representation in Curry
type RegExp a = [ORegExp a]
data ORegExp a = Nil
               | Literal a
               | Xor (RegExp a) (RegExp a)
               | Star (RegExp a)
               | Plus (RegExp a)
               | AnyLiteral
               | Bracket [Either a (a,a)]
               | NegBracket [Either a (a,a)]
               | Start (RegExp a)
               | End (RegExp a)
               | Times (Int,Int) (RegExp a)

--- The match function is used to match lists with regular expressions
--- @param r - The regular expression
--- @param s - The input list
--- @result True if matched else False
match :: Ord a => RegExp a -> [a] -> Bool
match re s = case re of
  []                  -> s == []
  (Nil:ors)           -> match ors s
  (Xor or1 or2:ors)   -> match (or1 ++ ors) s || match (or2 ++ ors) s
  (Literal c:ors)     -> case s of
    []      -> False
    (d:ds)  -> if (d == c) then match ors ds else False
  (Star r:ors)        -> matchstar r ors s
  (Plus r:ors)        -> matchplus r ors s
  (AnyLiteral:ors)    -> case s of
    []      -> False
    (_:ds)  -> match ors ds
  (Bracket b:ors)     -> case s of
    []      -> False
    (d:ds)  -> (matchbracket b d) && match ors ds
  (NegBracket b:ors)  -> case s of
    []      -> False
    (d:ds)  -> not (matchbracket b d) && match ors ds
  (Start _:_)         -> failed
  (End _:_)           -> failed
  (Times (n,m) r:ors) -> matchtimes s n m r ors

-- Matching with a star
matchstar :: Ord a => RegExp a -> RegExp a -> [a] -> Bool
matchstar r rgx st = (||)
  (match rgx st)
  (tryeach (map (\x -> match r x) (inits st)) (tails st) r rgx)

tryeach :: Ord a => [Bool] -> [[a]] -> RegExp a -> RegExp a -> Bool
tryeach [] []         _  _   = False
tryeach (b:bs) (t:ts) r  rgx = 
  (||)
    (if b
      then
        (match rgx t || matchstar r rgx t)
      else False)
    (tryeach bs ts r rgx)

-- Matching with a plus
matchplus :: Ord a => RegExp a -> RegExp a -> [a] -> Bool
matchplus r rgx st = tryeach (map (\x -> match r x) ini) tls r rgx
  where
    ini = tail (inits st)
    tls = tail (tails st)

-- Matching with a bracket
matchbracket :: Ord a => [Either a (a,a)] -> a -> Bool
matchbracket [] _                          = False
matchbracket (Left c:es)        d | c == d = True
                                  | c /= d = matchbracket es d
matchbracket (Right (c1,c2):es) d          =
  (||)
    (d >= c1 && d <= c2)
    (matchbracket es d)

-- Matching an amount of times between a range
matchtimes :: Ord a => [a] -> Int -> Int -> RegExp a -> RegExp a -> Bool
matchtimes s n m r rgx | m == 0 = match rgx s
                       | m >  0 =
  tryeachRestricted (m-n) (map (\x -> match mr x) (inits s)) (tails s) r rgx
  where
    mr = concat (replicate n r)

tryeachRestricted :: Ord a => Int -> [Bool] -> [[a]] -> RegExp a -> RegExp a
                  -> Bool
tryeachRestricted _      []     []     _   _   = False
tryeachRestricted m      (b:bs) (t:ts) r  rgx  =
  (||)
    (if b
      then
        (match rgx t || matchtimes t 1 m r rgx)  
      else False)
    (tryeachRestricted m bs ts r rgx)

------------------------------------------------------------------------------
