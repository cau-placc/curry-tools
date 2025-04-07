-- Tests for the RootReplaced analysis
--
-- Runt test with:
-- > cass RootReplaced RootReplacedTest.curry

loop :: a
loop = loop
--> root replacements: [loop] --> indicates infinite loop

f :: a -> [a]
f x = g x
--> root replacements: [g,h]

g :: a -> [a]
g x = h x
--> root replacements: [h]

h :: a -> [a]
h x = k x : []
--> root replacements: []

k :: a -> a
k x = x
--> root replacements: []
