module Benchmark where

import           Data.Trie 
import qualified Data.Map as M
import           Data.Maybe          ( fromJust )
import           Data.List           ( sum, maximum )
import           Debug.Profile       ( getTimingsNF )
import           Control.Applicative ( when ) 

-------------------------------------------------------------------------------
--- Profiling

--- Benchmarks construction time and random access time 
--- of lists, tries and maps.
profileLookup :: Bool -> Keys -> IO ()
profileLookup ll keys = do 
  let lAsList = zip keys [0..]
      lAsTrie = fromList   lAsList
      lAsMap  = M.fromList lAsList

  enforceNormalForm lAsList
  profile "Trie construction" lAsTrie
  profile "Map  construction" lAsMap

  when ll $ 
    profile "List lookup      " (sumAllL keys lAsList)
  profile   "Trie lookup      " (sumAllT keys lAsTrie)
  profile   "Map  lookup      " (sumAllM keys lAsMap)

--- Benchmarks random-access deletion time of all elements for tries and maps 
--- (naive deletion of the complete container).
profileDeletion :: Keys -> IO ()
profileDeletion keys = do 
  let lAsList = zip keys [0..]
      lAsTrie = fromList   lAsList
      lAsMap  = M.fromList lAsList

  enforceNormalForm (lAsList, lAsTrie, lAsMap)
  profile "Trie deletion    " (foldr delete   lAsTrie keys)
  profile "Map  deletion    " (foldr M.delete lAsMap  keys)

-- Runs the profiling tasks
main :: Prelude.IO ()
main = do 
  putStrLn "Benchmarking with small keys..."
  profileLookup True smallKeys

  putStrLn "\nBenchmarking with large keys..."
  profileLookup False largeKeys 

  putStrLn "\nBenchmarking deletion..."
  profileDeletion smallKeys 

{- Results:
>   Benchmarking with small keys...
>   Trie construction took 169ms
>   Map  construction took 1273ms
>   List lookup       took 13279ms
>   Trie lookup       took 89ms
>   Map  lookup       took 618ms
>   
>   Benchmarking with large keys...
>   Trie construction took 1526ms
>   Map  construction took 6946ms
>   Trie lookup       took 750ms
>   Map  lookup       took 5039ms
>   
>   Benchmarking deletion...
>   Trie deletion     took 486ms
>   Map  deletion     took 865ms
-}

-------------------------------------------------------------------------------
--- Benchmarking data
type Keys = [String]

alphabet :: [Char]
alphabet = ['a'..'j']

sizeAlphabet :: Int
sizeAlphabet = 10

toKey :: Int -> String
toKey n | n < 0            = error "toKey: negative number"
        | n < sizeAlphabet = [alphabet !! n]
        | otherwise        = toKey (n `div` sizeAlphabet) ++
                             toKey (n `mod` sizeAlphabet)

--- Small keys with length 1..3 and an alphabet of size 10.
smallKeys :: Keys
smallKeys = map toKey [1..3000]

--- Large keys of length 3..36 (rich alphabet).
largeKeys :: Keys
largeKeys = [ foldr1 (++) [a,b,c] | a <- as, b <- as, c <- as]
 where 
  as = ["These", "are", "some", "generic", "words", "to", "create", "large", "keys",
        "for", "benchmarking", "purposes", "in", "Curry", "programming", "language",
        "which", "is", "a", "functional", "logic", "language"]

lL :: [(String, Int)] -> String -> Int
lL   = (fromJust .) . flip Prelude.lookup 

sumAllL :: Keys -> [(String, Int)] -> Int
sumAllL keys = sum . flip map keys . lL 

lT :: Trie Int -> String -> Int
lT   = (fromJust .) . flip Data.Trie.lookup

sumAllT :: Keys -> Trie Int -> Int
sumAllT keys = sum . flip map keys . lT

lM :: M.Map String Int -> String -> Int
lM = (fromJust .) . flip M.lookup 

sumAllM :: Keys ->  M.Map String Int -> Int
sumAllM keys = sum . flip map keys . lM

-------------------------------------------------------------------------------
--- Auxiliary functions

enforceNormalForm :: a -> IO ()
enforceNormalForm x = normalForm x `seq` return ()

profile :: String -> a -> IO ()
profile nm exp = do 
  (rt, _, _) <- getTimingsNF exp
  putStrLn (nm ++ " took " ++ show rt ++ "ms")  