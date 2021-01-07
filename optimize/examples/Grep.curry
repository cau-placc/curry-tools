---------------------------------------------------------------
-- grep
---------------------------------------------------------------

import Test.Prop

-- Representation of regular expression:
data RE a = Lit a
          | Alt  (RE a) (RE a)
          | Conc (RE a) (RE a)
          | Star (RE a)

-- My characters:
data Chr = A | B | C | D | E
  deriving (Eq,Show)

-- Example: regular expression (ab*)
abstar :: RE Chr
abstar = Conc (Lit A) (Star (Lit B))

-- Example: regular expression (ab*c)
abstarc :: RE Chr
abstarc = Conc abstar (Lit C)

-- Semantics of regular expressions

sem :: RE a -> [a]
sem (Lit c)    = [c]
sem (Alt  a b) = sem a ? sem b
sem (Conc a b) = sem a ++ sem b
sem (Star a)   = [] ? sem (Conc a (Star a))

grep :: Data a => RE a -> [a] -> Bool
grep r s | _ ++ sem r ++ _ === s = True

bigABABC :: Int -> [Chr]
bigABABC n = take n (concatMap (\i -> A : take i (repeat B)) [1..]) ++ [A,B,C]

main :: Bool
main = grep abstarc (bigABABC 50)

test_grep = always $ grep abstarc (bigABABC 50)
