{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

import Test.EasyCheck

-- Dijsktra's Dutch National Flag problem with functional patterns

data Color = Red | White | Blue
 deriving (Eq,Show)

-- Formulation with default rule:
solveD :: [Color] -> [Color]
solveD (x++[White]++y++[Red  ]++z) = solveD (x++[Red  ]++y++[White]++z)
solveD (x++[Blue ]++y++[Red  ]++z) = solveD (x++[Red  ]++y++[Blue ]++z)
solveD (x++[Blue ]++y++[White]++z) = solveD (x++[White]++y++[Blue ]++z)
solveD'default flag = flag

iflag = [White,Red,White,Blue,Red,Blue,White]

main = solveD iflag
 --> [Red,Red,White,White,White,Blue,Blue]
 -- but also many more (identical) solutions!

test_solveD =  solveD iflag  <~>  [Red,Red,White,White,White,Blue,Blue]

-------------------------------------------------------------------------------
-- Sergio's version to obtain a single solution:
dutch :: [Color] -> [Color]
dutch (r@(uni Red) ++ w@(uni White) ++ b@(uni Blue) ++ (Red:xs))
  | w++b /= []  = dutch (Red:r ++ w ++ b ++ xs)
dutch (r@(uni Red) ++ w@(uni White) ++ b@(uni Blue) ++ (White:xs))
  | b /= []     = dutch (r ++ White:w ++ b ++ xs)
dutch'default z = z

uni color = [] ? color : uni color

testDutch =  dutch iflag  -=-  [Red,Red,White,White,White,Blue,Blue]
