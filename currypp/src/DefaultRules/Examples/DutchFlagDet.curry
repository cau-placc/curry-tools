{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

import Test.EasyCheck

-- Dijsktra's Dutch National Flag problem with functional patterns,
-- default rule declared as a deterministic function:

data Color = Red | White | Blue
 deriving (Eq,Show)

solveD :: [Color] -> DET [Color]
solveD (x++[White]++y++[Red  ]++z) = solveD (x++[Red  ]++y++[White]++z)
solveD (x++[Blue ]++y++[Red  ]++z) = solveD (x++[Red  ]++y++[Blue ]++z)
solveD (x++[Blue ]++y++[White]++z) = solveD (x++[White]++y++[Blue ]++z)
solveD'default flag = flag

iflag = [White,Red,White,Blue,Red,Blue,White]

main = solveD iflag
 --> [Red,Red,White,White,White,Blue,Blue]

test_solveD =  solveD iflag  -=-  [Red,Red,White,White,White,Blue,Blue]

