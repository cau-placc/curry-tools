-- An example to compute strongly connected compontents (SCCs).
-- In this example, we compute the SCCs w.r.t. the call graph
-- of a functional program. In order to support flexible graphs,
-- we model them by distinguishing the nodes and their labels.

import Data.SCC ( scc )

-- Node identifiers are simply integers.
type NodeID = Int

-- Nodes are labeled with strings.
type Label = String

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

-- The labels of the nodes:
label :: NodeID -> Label
label 1 = "f"
label 2 = "g"
label 3 = "h"
label 4 = "i"
label 5 = "j"
label 6 = "k"
label 7 = "l"

-- This operation models the call-graph, i.e., it associates to a function
-- node the functions called by this function:
calls :: NodeID -> [Label]
calls 1 = ["g","h"]
calls 2 = ["f"]
calls 3 = ["g","i"]
calls 4 = []
calls 5 = ["k"]
calls 6 = ["j","l"]
calls 7 = []

-- We compute the SCCs:
main :: [[Label]]
main = map (map label) $ scc (\n -> [label n]) calls [1..7]
-- Result: [["i"],["f","g","h"],["l"],["j","k"]]
-- (note that the result is not ordered, e.g., topologically)
