{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

import Test.EasyCheck

-- A simple implementation of a two-person game (here: Nim) with default rules
--
-- Example taken from (where the same example was implemented with "fails"):
-- F.J. Lopez-Fraguas, J. Sanchez-Hernandez:
-- A Proof Theoretic Approach to Failure in Functional Logic Programming
-- TPLP 4(1), 2004

-- A winning move is some move of the game to a state
-- where the other person can *not* win:
winMove state | next == move state = proceed state next   where next free

-- We proceed with a next state only if the next state does not allow
-- a winning move:
proceed state next | winMove next == _ = failed
proceed'default _ next = next

-- Here are the moves of the Nim game: pick some number of sticks
-- from *one* row, where the rows are presented as a list of (Peano)
-- numbers (representing the number of sticks):
move (x:xs) = (pick x : xs) ? (x : move xs)

pick (S n) = n ? pick n

data Nat = Z | S Nat
 deriving (Eq,Show)


main = winMove [S (S Z), S Z]

testNim = winMove [S (S Z), S Z]  -=-  [S Z, S Z]
