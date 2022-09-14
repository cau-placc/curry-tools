-- An example to compute strongly connected compontents (SCCs).
-- In this example, we compute the SCCs w.r.t. a given call graph
-- of a functional program.

import Data.SCC ( scc )

{-
As an example, consider the following Haskell program:

f n  =  g n + h n
g n  =  j n + f n
h n  =  i n + g n
i n  =  n
j n  =  k n
k n  =  j n + l n
l n  =  5
-}

-- We model the call graph by a data type representing the functions:
data Funcs = F | G | H | I | J | K | L
 deriving Eq

-- This operation models the call-graph, i.e., it associates to a function
-- the functions called by this function:
calls :: Funcs -> [Funcs]
calls F = [G,H]
calls G = [F]
calls H = [G,I]
calls I = []
calls J = [K]
calls K = [J,L]
calls L = []

-- We compute the SCCs:
main :: [[Funcs]]
main = scc (\x -> [x]) calls [F,G,H,I,J,K,L]
-- Result: [[I],[F,G,H],[L],[J,K]]
-- (note that the result is not ordered, e.g., topologically)
