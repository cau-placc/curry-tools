import Test.Prop
import SearchTreeGenerators

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

-- Unit tests:
revNull = rev []      -=- ([] :: [Int])
rev123  = rev [1,2,3] -=- [3,2,1]

-- Property: reversing two times is the identity:
revRevIsId xs = rev (rev xs) -=- xs

-- Non-deterministic list insertion:
insert :: a -> [a] -> [a]
insert x xs     = x : xs
insert x (y:ys) = y : insert x ys

insertAsFirstOrLast :: Int -> [Int] -> Prop
insertAsFirstOrLast x xs = insert x xs ~> (x:xs ? xs++[x])

-- A permutation of a list:
perm :: [a] -> [a]
perm []     = []
perm (x:xs) = insert x (perm xs)

permLength :: [Int] -> Prop
permLength xs = length (perm xs) <~> length xs

permCount :: [Int] -> Prop
permCount xs = allDifferent xs ==> perm xs # fac (length xs)
 where
   fac n = foldr (*) 1 [1..n]

allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

-- Is a list sorted?
sorted :: [Int] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:zs) = x<=y && sorted (y:zs)

permIsEventuallySorted :: [Int] -> Prop
permIsEventuallySorted xs = eventually $ sorted (perm xs)

-- Permutation sort:
psort :: [Int] -> [Int]
psort xs | sorted ys = ys
 where ys = perm xs

psortIsAlwaysSorted xs = always $ sorted (psort xs)

psortKeepsLength xs = length (psort xs) <~> length xs

-- Quicksort:
qsort :: [Int] -> [Int] 
qsort []     = []
qsort (x:l)  = qsort (filter (<x) l) ++ x : qsort (filter (>=x) l)

qsortIsSorting xs = qsort xs <~> psort xs

--------------------------------------------------------------------------
-- Generating test data:

neg_or b1 b2 = not (b1 || b2) -=- not b1 && not b2


-- Natural numbers defined by s-terms (Z=zero, S=successor):
data Nat = Z | S Nat
 deriving (Eq,Show)

-- addition on natural numbers:
add :: Nat -> Nat -> Nat
add Z     n = n
add (S m) n = S(add m n)

-- Property: the addition operator is commutative
addIsCommutative x y = add x y -=- add y x

-- Property: the addition operator is associative
addIsAssociative x y z = add (add x y) z -=- add x (add y z)


-- A general tree type:
data Tree a = Leaf a | Node [Tree a]
 deriving (Eq,Show)

-- The leaves of a tree:
leaves (Leaf x) = [x]
leaves (Node ts) = concatMap leaves ts

-- Mirror a tree:
mirror (Leaf x) = Leaf x
mirror (Node ts) = Node (reverse (map mirror ts))

-- Property: double mirroring is the identity
doubleMirror t = mirror (mirror t) -=- t

-- Property: the leaves of a mirrored are in reverse order
leavesOfMirrorAreReversed t = leaves t -=- reverse (leaves (mirror t))

-- Sum up all numbers:
sumUp n = if n==0 then 0 else n + sumUp (n-1)

sumUpIsCorrect n = n>=0 ==> sumUp n -=- n * (n+1) `div` 2

-- To test sumUpIsCorrect explicitly on non-ngeative integers,
-- we define a new data type to wrap integers:
data NonNeg = NonNeg { nonNeg :: Int }
 deriving (Eq,Show)

-- We define our own generator for producing only non-negative integers:
genNonNeg = genCons1 NonNeg genNN
 where
   genNN = genCons0 0 ||| genCons1 (\n -> 2*(n+1)) genNN
                      ||| genCons1 (\n -> 2*n+1)   genNN

-- Now we write our own test:
sumUpIsCorrectOnNonNeg = sumUpIsCorrect . nonNeg

