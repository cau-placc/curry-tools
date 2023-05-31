import Data.GraphViz

-- A simple undirected graph without attributes.
graph1 :: DotGraph
graph1 = ugraph "Simple"
  [Node "a" [], Node "b" [], Node "c" []]
  [Edge "a" "b" [], Edge "b" "c" [], Edge "c" "a" []]

test1 :: IO ()
test1 = viewDotGraph graph1

-- A simple directed graph without attributes.
graph2 :: DotGraph
graph2 = dgraph "Simple"
 [Node "a" [], Node "b" [], Node "c" []]
 [Edge "a" "b" [], Edge "b" "c" [], Edge "c" "a" []]

test2 :: IO ()
test2 = viewDotGraph graph2

-- Another simple directed graph with some attributes.
graph3 :: DotGraph
graph3 = dgraphWithAttrs "Simple"
  [("bgcolor","lightyellow"), ("pad","0.5")]
  [Node "a" nattrs, Node "b" nattrs, Node "c" nattrs]
  [Edge "a" "b" [], Edge "b" "c" [],
   Edge "c" "a" [("dir","none"),("label","back")]]
 where
  nattrs = [("shape","record"),("style","bold"),
            ("label","{Label|line1\\nline2}")]

test3 :: IO ()
test3 = viewDotGraph graph3
