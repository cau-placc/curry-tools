{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

import Test.EasyCheck

-- The implementation of coloring a map using a default rule
-- (by Sergio Antoy).

data State = WA | OR | ID | BC
 deriving (Eq,Show)

states = [WA,OR,ID,BC]

adjacent = [(WA,OR),(WA,ID),(WA,BC),(OR,ID),(ID,BC)]

data Color = Red | Green | Blue
 deriving (Eq,Show)

color x = (x, Red ? Green ? Blue)

-- it is a failure if two states with the same color are adjacent:
solve (_++[(s1,c)]++_++[(s2,c)]++_) (_++[(s1,s2)]++_) = failed
solve'default x _ = x

main = solve (map color states) adjacent

solveMap = main <~>
         ( [(WA,Red),(OR,Green),(ID,Blue),(BC,Green)]
         ? [(WA,Red),(OR,Blue),(ID,Green),(BC,Blue)]
         ? [(WA,Green),(OR,Red),(ID,Blue),(BC,Red)]
         ? [(WA,Green),(OR,Blue),(ID,Red),(BC,Blue)]
         ? [(WA,Blue),(OR,Red),(ID,Green),(BC,Red)]
         ? [(WA,Blue),(OR,Green),(ID,Red),(BC,Green)]
         )
