------------------------------------------------------------------------------
--- Simple profiling library with operations to access run-time data.
---
--- @author Michael Hanus
--- @version January 2019
------------------------------------------------------------------------------

module Debug.Profile
  ( ProcessInfo(..), getProcessInfos, showMemInfo, printMemInfo
  , garbageCollectorOff, garbageCollectorOn, garbageCollect
  , profileTime, profileTimeNF, profileSpace, profileSpaceNF
  , getTimings, getTimingsNF
  )
 where

import Data.List (intersperse)

--- The data type for representing information about the state
--- of a Curry process.
--- @cons RunTime - the run time in milliseconds
--- @cons ElapsedTime - the elapsed time in milliseconds
--- @cons Memory - the total memory in bytes
--- @cons Code - the size of the code area in bytes
--- @cons Stack - the size of the local stack for recursive functions in bytes
--- @cons Heap - the size of the heap to store term structures in bytes
--- @cons Choices - the size of the choicepoint stack
--- @cons GarbageCollections - the number of garbage collections performed
data ProcessInfo = RunTime | ElapsedTime | Memory | Code
                 | Stack | Heap | Choices | GarbageCollections
 deriving (Eq,Show)

--- Returns various informations about the current state of the Curry process.
--- Note that the returned values are implementation dependent
--- so that one should interpret them with care!
---
--- Note for KiCS2 users:
--- Since GHC version 7.x, one has to set the run-time option `-T`
--- when this operation is used. This can be done by the kics2 command
---
---     :set rts -T
---
getProcessInfos :: IO [(ProcessInfo,Int)]
getProcessInfos external

--- Turns off the garbage collector of the run-time system (if possible).
--- This could be useful to get more precise data of memory usage.
garbageCollectorOff :: IO ()
garbageCollectorOff external

--- Turns on the garbage collector of the run-time system (if possible).
garbageCollectorOn :: IO ()
garbageCollectorOn external

--- Invoke the garbage collector (if possible).
--- This could be useful before run-time critical operations.
garbageCollect :: IO ()
garbageCollect external

--- Get a human readable version of the memory situation from the
--- process infos.
showMemInfo :: [(ProcessInfo,Int)] -> String
showMemInfo infos = concat $ intersperse ", " $
  formatItem Memory "Memory: "  ++
  formatItem Code   "Code: "    ++
  formatItem Stack  "Stack: "   ++
  formatItem Choices"Choices: " ++
  formatItem Heap   "Heap: "
 where
   formatItem i s = maybe [] (\v -> [s ++ showBytes v]) (lookup i infos)

   showBytes b = if b<10000 then show b
                            else show (b `div` 1000) ++ " kb"

--- Print a human readable version of the current memory situation
--- of the Curry process.
printMemInfo :: IO ()
printMemInfo = getProcessInfos >>= putStrLn . showMemInfo

--- Print the time needed to execute a given IO action.
profileTime :: IO a -> IO a
profileTime action = do
  (result,rt,et,gc) <- getTimings action
  putStrLn $ "Run time:            " ++ show rt ++ " msec."
  putStrLn $ "Elapsed time:        " ++ show et ++ " msec."
  putStrLn $ "Garbage collections: " ++ show gc
  return result

--- Returns the run time, elapsed time, and number of garbage collections
--- needed to execute a given IO action.
getTimings :: IO a -> IO (a,Int,Int,Int)
getTimings action = do
  garbageCollect
  pi1 <- getProcessInfos
  result <- action
  pi2 <- getProcessInfos
  return (result,
          infoDiff pi1 pi2 RunTime,
          infoDiff pi1 pi2 ElapsedTime,
          infoDiff pi1 pi2 GarbageCollections)

--- Evaluates the argument to normal form
--- and print the time needed for this evaluation.
profileTimeNF :: a -> IO ()
profileTimeNF exp = profileTime (seq (id $!! exp) (return ()))

--- Evaluates the argument to normal form
--- and returns the run time, elapsed time, and number of garbage collections
--- needed for this evaluation.
getTimingsNF :: a -> IO (Int,Int,Int)
getTimingsNF exp = do
  (_,rt,et,gc) <- getTimings (seq (id $!! exp) (return ()))
  return (rt,et,gc)

--- Print the time and space needed to execute a given IO action.
--- During the executation, the garbage collector is turned off to get the
--- total space usage.
profileSpace :: IO a -> IO a
profileSpace action = do
  garbageCollect
  garbageCollectorOff
  pi1 <- getProcessInfos
  result <- action
  pi2 <- getProcessInfos
  garbageCollectorOn
  putStrLn $ "Run time:            "
             ++ (showInfoDiff pi1 pi2 RunTime) ++ " msec."
  putStrLn $ "Elapsed time:        "
             ++ (showInfoDiff pi1 pi2 ElapsedTime) ++ " msec."
  putStrLn $ "Garbage collections: "
             ++ (showInfoDiff pi1 pi2 GarbageCollections)
  putStrLn $ "Heap usage:          "
             ++ (showInfoDiff pi1 pi2 Heap) ++ " bytes"
  putStrLn $ "Stack usage:         "
             ++ (showInfoDiff pi1 pi2 Stack) ++ " bytes"
  return result

--- Evaluates the argument to normal form
--- and print the time and space needed for this evaluation.
--- During the evaluation, the garbage collector is turned off to get the
--- total space usage.
profileSpaceNF :: a -> IO ()
profileSpaceNF exp = profileSpace (seq (id $!! exp) (return ()))

showInfoDiff :: [(ProcessInfo, Int)] -> [(ProcessInfo, Int)] -> ProcessInfo
             -> String
showInfoDiff infos1 infos2 item =
  show (maybe 0 id (lookup item infos2) - maybe 0 id (lookup item infos1))

infoDiff :: [(ProcessInfo, Int)] -> [(ProcessInfo, Int)] -> ProcessInfo -> Int
infoDiff infos1 infos2 item =
  maybe 0 id (lookup item infos2) - maybe 0 id (lookup item infos1)
