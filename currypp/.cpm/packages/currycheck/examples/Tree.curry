-- Some examples for the use of CurryCheck with user-defined data

import Test.Prop

-- A general tree type:
data Tree a = Leaf a | Node [Tree a]
 deriving (Eq,Show)

leaves (Leaf x) = [x]
leaves (Node ts) = concatMap leaves ts

mirror (Leaf x) = Leaf x
mirror (Node ts) = Node (reverse (map mirror ts))


-- Property: double mirroring is the identity
mirror_mirror t = mirror (mirror t) -=- t

-- Property: the leaves of a mirrored are in reverse order
leaves_mirror t = leaves t -=- reverse (leaves (mirror t))
