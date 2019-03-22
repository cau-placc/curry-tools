-------------------------------------------------------------------------
--- EasyCheck is a library for automated, property-based testing of
--- Curry programs. The ideas behind EasyCheck are described in
--- [this paper](http://www-ps.informatik.uni-kiel.de/~sebf/pub/flops08.html).
--- The CurryCheck tool automatically executes tests defined with
--- this library. CurryCheck supports the definition of unit tests
--- (also for I/O operations) and property tests parameterized
--- over some arguments. CurryCheck is described in more detail in
--- [this paper](http://www.informatik.uni-kiel.de/~mh/papers/LOPSTR16.html).
---
--- Note that this module defines the interface of EasyCheck to
--- define properties. The operations to actually execute the tests
--- are contained in the accompanying library `Test.EasyCheckExec`.
---
--- @author Sebastian Fischer (with extensions by Michael Hanus)
--- @version June 2016
--- @category general
-------------------------------------------------------------------------

module Test.EasyCheck (

  -- test specification:
  PropIO, returns, sameReturns, toError, toIOError,

  Test, Prop, (==>), for, forAll,

  test, is, isAlways, isEventually, uniquely, always, eventually,
  failing, successful, deterministic, (-=-), (<~>), (~>), (<~), (<~~>),
  (#), (#<), (#>), (<=>),
  solutionOf,

  -- test annotations
  label, trivial, classify, collect, collectAs,

  -- enumerating values
  valuesOfSearchTree, valuesOf,

  -- for EasyCheckExec
  Result(..), result, args, updArgs, stamp, testsOf, ioTestOf, forAllValues

  ) where

import Findall              (getAllValues)
import Data.List            ( (\\), delete, diagonal, nub )
import SearchTree           ( SearchTree, someSearchTree )
import SearchTree.Traversal

infix  1 `is`, `isAlways`, `isEventually`
infix  1 -=-, <~>, ~>, <~, <~~>, `trivial`, #, #<, #>, <=>
infix  1 `returns`, `sameReturns`
infixr 0 ==>


-------------------------------------------------------------------------
--- Abstract type to represent properties involving IO actions.
data PropIO = PropIO (Bool -> String -> IO (Maybe String))

--- The property `returns a x` is satisfied if the execution of the
--- I/O action `a` returns the value `x`.
returns :: (Eq a, Show a) => IO a -> a -> PropIO
returns act r = PropIO (testIO act (return r))

--- The property `sameReturns a1 a2` is satisfied if the execution of the
--- I/O actions `a1` and `a2` return identical values.
sameReturns :: (Eq a, Show a) => IO a -> IO a -> PropIO
sameReturns a1 a2 = PropIO (testIO a1 a2)

--- The property `toError a` is satisfied if the evaluation of the argument
--- to normal form yields an exception.
toError :: a -> PropIO
toError x = toIOError (getAllValues x >>= \rs -> (id $!! rs) `seq` done)

--- The property `toIOError a` is satisfied if the execution of the
--- I/O action `a` causes an exception.
toIOError :: IO a -> PropIO
toIOError act = PropIO (hasIOError act)

--- Extracts the tests of an I/O property (used by the test runner).
ioTestOf :: PropIO -> (Bool -> String -> IO (Maybe String))
ioTestOf (PropIO t) = t

-- Test an IO property, i.e., compare the results of two IO actions.
testIO :: (Eq a, Show a) => IO a -> IO a -> Bool -> String -> IO (Maybe String)
testIO act1 act2 quiet msg =
   catch (do r1 <- act1
             r2 <- act2
             if r1 == r2
               then unless quiet (putStr (msg++": OK\n")) >> return Nothing
               else do putStrLn $ msg++": FAILED!\nResults: " ++ show (r1,r2)
                       return (Just msg)
         )
         (\err -> do putStrLn $ msg++": EXECUTION FAILURE:\n" ++ showError err
                     return (Just msg)
         )

-- Test whether an IO action produces an error.
hasIOError :: IO a -> Bool -> String -> IO (Maybe String)
hasIOError act quiet msg =
   catch (act >> return (Just msg))
         (\_ -> unless quiet (putStr (msg++": OK\n")) >> return Nothing)

-------------------------------------------------------------------------
--- Abstract type to represent a single test for a property to be checked.
--- A test consists of the result computed for this test,
--- the arguments used for this test, and the labels possibly assigned
--- to this test by annotating properties.
data Test = Test Result [String] [String]

--- Data type to represent the result of checking a property.
data Result = Undef | Ok | Falsified [String] | Ambigious [Bool] [String]

--- Abstract type to represent properties to be checked.
--- Basically, it contains all tests to be executed to check the property.
data Prop = Prop [Test]

--- Extracts the tests of a property (used by the test runner).
testsOf :: Prop -> [Test]
testsOf (Prop ts) = ts

--- An empty test.
notest :: Test
notest = Test Undef [] []

--- Extracts the result of a test.
result :: Test -> Result
result (Test r _ _) = r

--- Set the result of a test.
setResult :: Result -> Test -> Test
setResult res (Test _ s a) = Test res a s

--- Extracts the arguments of a test.
args :: Test -> [String]
args  (Test _ a _) = a

--- Extracts the labels of a test.
stamp :: Test -> [String]
stamp (Test _ _ s) = s

--- Updates the arguments of a test.
updArgs :: ([String] -> [String]) -> Test -> Test
updArgs  upd (Test r a s) = Test r (upd a) s

--- Updates the labels of a test.
updStamp :: ([String] -> [String]) -> Test -> Test
updStamp upd (Test r a s) = Test r a (upd s)

-- Test Specification

--- Constructs a property to be tested from an arbitrary expression
--- (first argument) and a predicate that is applied to the list of
--- non-deterministic values. The given predicate determines whether
--- the constructed property is satisfied or falsified for the given
--- expression.
test :: Show a => a -> ([a] -> Bool) -> Prop
test x f = Prop [setResult res notest]
 where
  xs  = valuesOf x
  res = case valuesOf (f xs) of
          [True]  -> Ok
          [False] -> Falsified (map show xs)
          bs      -> Ambigious bs (map show xs)

--- The property `x -=- y` is satisfied if `x` and `y` have deterministic
--- values that are equal.
(-=-) :: (Eq a, Show a) => a -> a -> Prop
x -=- y = (x,y) `is` uncurry (==)

--- The property `x <~> y` is satisfied if the sets of the values of
--- `x` and `y` are equal.
(<~>) :: (Eq a, Show a) => a -> a -> Prop
x <~>  y = test x (isSameSet (valuesOf y))

--- The property `x ~> y` is satisfied if `x` evaluates to every value of `y`.
--- Thus, the set of values of `y` must be a subset of the set of values of `x`.
(~>) :: (Eq a, Show a) => a -> a -> Prop
x  ~>  y = test x (isSubsetOf (valuesOf y))

--- The property `x <~ y` is satisfied if `y` evaluates to every value of `x`.
--- Thus, the set of values of `x` must be a subset of the set of values of `y`.
(<~) :: (Eq a, Show a) => a -> a -> Prop
x  <~  y = test x (`isSubsetOf` (valuesOf y))

--- The property `x <~~> y` is satisfied if the multisets of the values of
--- `x` and `y` are equal.
(<~~>) :: (Eq a, Show a) => a -> a -> Prop
x <~~> y = test x (isSameMSet (valuesOf y))

isSameSet :: Eq a => [a] -> [a] -> Bool
isSameSet xs ys = xs' `subset` ys' && ys' `subset` xs'
 where xs' = nub xs
       ys' = nub ys

isSubsetOf :: Eq a => [a] -> [a] -> Bool
xs `isSubsetOf` ys = nub xs `subset` ys

subset :: Eq a => [a] -> [a] -> Bool
xs `subset` ys = null (xs\\ys)

-- compare to lists if they represent the same multi-set
isSameMSet :: Eq a => [a] -> [a] -> Bool
isSameMSet []     ys = ys == []
isSameMSet (x:xs) ys
  | x `elem` ys  = isSameMSet xs (delete x ys)
  | otherwise    = False

--- A conditional property is tested if the condition evaluates to `True`.
(==>) :: Bool -> Prop -> Prop
cond ==> p =
  if True `elem` valuesOf cond
  then p
  else Prop [notest]

--- `solutionOf p` returns (non-deterministically) a solution
--- of predicate `p`. This operation is useful to test solutions
--- of predicates.
solutionOf :: (a -> Bool) -> a
solutionOf pred = pred x &> x where x free

--- The property `is x p` is satisfied if `x` has a deterministic value
--- which satisfies `p`.
is :: Show a => a -> (a -> Bool) -> Prop
is x f = test x (\xs -> case xs of [y] -> f y
                                   _   -> False)

--- The property `isAlways x p` is satisfied if all values of `x` satisfy `p`.
isAlways :: Show a => a -> (a -> Bool) -> Prop
isAlways x  = test x . all

--- The property `isEventually x p` is satisfied if some value of `x`
--- satisfies `p`.
isEventually :: Show a => a -> (a -> Bool) -> Prop
isEventually x = test x . any

--- The property `uniquely x` is satisfied if `x` has a deterministic value
--- which is true.
uniquely :: Bool -> Prop
uniquely = (`is` id)

--- The property `always x` is satisfied if all values of `x` are true.
always :: Bool -> Prop
always = (`isAlways` id)

--- The property `eventually x` is satisfied if some value of `x` is true.
eventually :: Bool -> Prop
eventually = (`isEventually` id)

--- The property `failing x` is satisfied if `x` has no value.
failing :: Show a => a -> Prop
failing x = test x null

--- The property `successful x` is satisfied if `x` has at least one value.
successful :: Show a => a -> Prop
successful x = test x (not . null)

--- The property `deterministic x` is satisfied if `x` has exactly one value.
deterministic :: Show a => a -> Prop
deterministic x = x `is` const True

--- The property `x # n` is satisfied if `x` has `n` values.
(#) :: (Eq a, Show a) => a -> Int -> Prop
x # n = test x ((n==) . length . nub)

--- The property `x #< n` is satisfied if `x` has less than `n` values.
(#<) :: (Eq a, Show a) => a -> Int -> Prop
x #< n = test x ((<n) . length . nub)

--- The property `x #> n` is satisfied if `x` has more than `n` values.
(#>) :: (Eq a, Show a) => a -> Int -> Prop
x #> n = test x ((>n) . length . nub)

--- The property `for x p` is satisfied if all values `y` of `x`
--- satisfy property `p y`.
for :: Show a => a -> (a -> Prop) -> Prop
for x p = forAll (valuesOf x) p

--- The property `forAll xs p` is satisfied if all values `x` of the list `xs`
--- satisfy property `p x`.
forAll :: Show a => [a] -> (a -> Prop) -> Prop
forAll xs p = forAllValues id xs p

--- Only for internal use by the test runner.
forAllValues :: Show a => (b -> Prop) -> [a] -> (a -> b) -> Prop
forAllValues c vals f =
 Prop $
  diagonal
    [[ updArgs (show y:) t | let Prop ts = c (f y), t <- ts ] | y <- vals ]

--- The property `f <=> g` is satisfied if `f` and `g` are equivalent
--- operations, i.e., they can be replaced in any context without changing
--- the computed results.
(<=>) :: a -> a -> Prop
_ <=> _ = error $
  "Test.Prop.<=> not executable. Use CurryCheck to test this property!"

-------------------------------------------------------------------------
-- Test Annotations

--- Assign a label to a property.
--- All labeled tests are counted and shown at the end.
label :: String -> Prop -> Prop
label l (Prop ts) = Prop (map (updStamp (l:)) ts)

--- Assign a label to a property if the first argument is `True`.
--- All labeled tests are counted and shown at the end.
--- Hence, this combinator can be used to classify tests:
---
---     multIsComm x y = classify (x<0 || y<0) "Negative" $ x*y -=- y*x
---
classify :: Bool -> String -> Prop -> Prop
classify True  name = label name
classify False _    = id

--- Assign the label "trivial" to a property if the first argument is `True`.
--- All labeled tests are counted and shown at the end.
trivial :: Bool -> Prop -> Prop
trivial = (`classify` "trivial")

--- Assign a label showing the given argument to a property.
--- All labeled tests are counted and shown at the end.
collect :: Show a => a -> Prop -> Prop
collect = label . show

--- Assign a label showing a given name and the given argument to a property.
--- All labeled tests are counted and shown at the end.
collectAs :: Show a => String -> a -> Prop -> Prop
collectAs name = label . ((name++": ")++) . show

-------------------------------------------------------------------------
-- Value generation

--- Extracts values of a search tree according to a given strategy
--- (here: randomized diagonalization of levels with flattening).
valuesOfSearchTree :: SearchTree a -> [a]
valuesOfSearchTree
  -- = depthDiag
  -- = rndDepthDiag 0
  -- = levelDiag
  -- = rndLevelDiag 0
  = rndLevelDiagFlat 5 0
  -- = allValuesB

--- Computes the list of all values of the given argument
--- according to a given strategy (here:
--- randomized diagonalization of levels with flattening).
valuesOf :: a -> [a]
valuesOf = valuesOfSearchTree . someSearchTree . (id $##)

-------------------------------------------------------------------------
