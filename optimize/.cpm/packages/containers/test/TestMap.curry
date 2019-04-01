import Data.List  hiding (union)
import Data.Maybe
import Prelude    hiding (lookup)

import System.Random
import Test.Prop

import Data.Map

fm f = f . fromList . map (\x ->(x,x))

fms f = map fst . toList . fm f

fms' f = map snd . toList . fm f

so f = spnub . sortBy (<) . f

testInsert = eq (fms (\x-> insert 73 73 x)) (so (73:))

testDeleteAll = test
  (\nums -> fms (deleteAll (take 500 nums)) nums == so (drop 500) nums)

testUnion =
 test (\nums -> let l=length nums
                    (xs,ys) = splitAt (div l 2) nums
         in (map fst $ toList $ union (fm id xs) (fm id ys)) == so id nums)

testDifference =
 test (\nums -> let l = length nums
                    (xs,ys) = splitAt (div l 2) nums
         in (map fst $ toList $ difference (fm id nums) (fm id ys)) == so id xs)

testIntersection = test
   (\nums -> let l=length nums
                 (_,ys) = splitAt (div l 2) nums
     in (map fst $ toList $ intersection (fm id ys) (fm id nums)) == so id ys)

testFoldrWithKey = eq (fm (foldrWithKey (\x _ z->x+z) 0)) (foldl (+) 0)

testMapWithKey = eq (fms' (mapWithKey (\_ z->z+1))) (so (map (+1)))

testFilterFM = eq (fms (filterWithKey (\x _->x>0))) (so (filter (>0)))

testSize = eq (fm size) length

testMember_Lookup = eq (fm (\x-> member 73 (insert 73 73 x))) (const True)

testKeys_elems = test
         (\nums -> let finm=fm id nums in unzip (toList finm)==(keys finm, elems finm))

testSortWithMap = eq sortWithMap (so id)

testMin_Max = eq (fm (\finm -> (fst $ fromJust $ lookupMin finm,
                                          fst $ fromJust $ lookupMax finm)))
                           ((\l->(head l,last l)) .so id)

testAdjust = eq (fm (\x-> lookup 73 (adjust (+7) 73 (insert 73 73 x)))) (const $ Just 80)


spnub [] = []
spnub [x] = [x]
spnub (x:y:xs) = if x==y then spnub (y:xs) else x:spnub (y:xs)


------------------------------------------------------------------------------
-- Random test:

--- Tests a given predicate on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
test :: ([Int] -> Bool) -> PropIO
test f =
  (rndList lenRnds >>= \xs -> return (if f xs then Nothing else Just xs))
  `returns` Nothing

--- Tests whether two operations return equal results
--- on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
eq :: Eq a => ([Int] -> a) -> ([Int] -> a) -> PropIO
eq f g = test (\x -> (f x)==(g x))

--- generate a list of at most n random numbers (without duplicated elements)
rndList :: Int -> IO [Int]
rndList n = getRandomSeed >>= return . nub . take n . (flip nextIntRange 100000)

--- maximal length of test lists
lenRnds :: Int
lenRnds = 1000

------------------------------------------------------------------------------
