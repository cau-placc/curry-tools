---------------------------------------------------------------------
-- Specifying regular expressions and their semantics.
-- 
-- This example is taken from:
-- S. Antoy, M. Antoy: Functional Logic Programming
-- Comm. of the ACM, 53(4), pp. 74-85
---------------------------------------------------------------------

module RegExpSem where

--- A data type to represent regular expression over some alphabet.
--- A regular expression is either a literal, i.e., a member of the alphabet,
--- an choice between two regular expressions, the concatenation
--- of two regular expressions, or the repetition of a regular expression.
data RE a = Lit a
          | Alt  (RE a) (RE a)
          | Conc (RE a) (RE a)
          | Star (RE a)

--- We can extend the language of regular expressions by standard abstractions.
--- Here we introduce an operator denoting at least one or more repetitions
--- of a regular expression:
plus :: RE a -> RE a
plus re = Conc re (Star re)

--- The semantics of regular expressions can be defined as a nondeterministic
--- operation associating any word of the language defined by the regular
--- expression:
sem :: RE a -> [a]
sem (Lit c)    = [c]
sem (Alt  a b) = sem a ? sem b
sem (Conc a b) = sem a ++ sem b
sem (Star a)   = [] ? sem (Conc a (Star a))

--- An operation to match a string against a regular expression
--- can be defined by the following constraint:
match :: RE a -> [a] -> Bool
match r s = sem r =:= s

-- A constraint similar to Unix's grep (i.e., to check whether a regular
-- expression is contained somewhere in a string) can be defined
-- as follows:
grep :: RE a -> [a] -> Bool
grep r s = _ ++ sem r ++ _ =:= s

--- The following operation extends the operation 'grep' to return
--- the substring which starts with the regular expression.
grepShow :: RE a -> [a] -> [a]
grepShow r s | xs ++ sem r ++ _ =:= s  = drop (length xs) s
   where xs free

--- The following operation extends the operation 'grep' to return
--- the position where the matched regular expression starts.
grepPos :: RE a -> [a] -> Int
grepPos r s | xs ++ sem r ++ ys =:= s
            = length xs
 where xs,ys free
