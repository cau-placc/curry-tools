------------------------------------------------------------------------
--- Implementation of the analysis computations on the server side
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version March 2021
------------------------------------------------------------------------

-- analysis computations on the server side

module CASS.ServerFunctions where

import System.IO          ( Handle(..), hClose, hFlush, hGetLine, hPutStrLn
                          , hWaitForInput, hWaitForInputs )
import System.Process     ( system, sleep )
import System.Directory   ( doesFileExist, getModificationTime )
import Data.Maybe         ( fromMaybe )
import Data.List          ( delete )
import Data.Time          ( ClockTime )
import XML                ( showXmlDoc, xml )

import FlatCurry.Types    ( QName )
import FlatCurry.Goodies  ( progImports )
import Analysis.Logging   ( debugMessage )
import Analysis.Types
import Analysis.ProgInfo
import CASS.Dependencies
import CASS.Configuration ( CConfig, debugLevel, waitTime )

data WorkerMessage = Task String String | ChangePath String | StopWorker
 deriving (Read, Show)

-- Master loop for communication with workers
-- Argument 1: handles for workers that are currently free
-- Argument 2: handles for workers that are currently busy
-- Argument 3: the analysis name
-- Argument 4: the name of the main module
-- Argument 5: the modules to be analyzed (with their dependencies)
-- Argument 6: names of modules that are ready be to analyzed (since their
--             imports are already analyzed)
-- Result: Nothing (in case of successful work) or (Just <error>)
masterLoop :: CConfig -> [Handle] -> [Handle] -> String -> String
           -> [(String,[String])] -> [String] -> IO (Maybe String)
masterLoop cc _ [] _ _ [] [] = do
  debugMessage (debugLevel cc) 2 "Master loop: terminated"
  return Nothing

masterLoop cc _ (b:busyWorker) ananame mainModule [] [] = do
  debugMessage dl 2 "Master loop: waiting for worker result"
  inputHandle <- hWaitForInputs (b:busyWorker) waitTime
  if inputHandle/=0
    then return (Just "No input from any worker received")
    else do
      let handle =  b
      input <- hGetLine handle
      debugMessage dl 2 ("Master loop: got message: "++input)
      let Task ananame2 moduleName2 = read input
      if ananame == ananame2 && moduleName2 == mainModule
        then return Nothing
        else return (Just "Received analysis does not match requested analysis")
 where dl = debugLevel cc

masterLoop cc idleWorker busyWorker ananame mainModule
           modulesToDo@(_:_) [] = do
  debugMessage dl 3 ("Master loop: modules to do: " ++ show modulesToDo)
  let modulesToDo2 = filter ((not . null) . snd) modulesToDo
      waitList     = map fst (filter (null . snd) modulesToDo)
  if null waitList
    then do
      debugMessage dl 2 "Master loop: waiting for workers to finish"
      inputHandle <- hWaitForInputs busyWorker waitTime
      if inputHandle<0
        then return (Just "No input from any worker received")
        else do
          let handle =  busyWorker !! inputHandle
          input <- hGetLine handle
          debugMessage dl 2 ("Master loop: got message: " ++ input)
          let Task ananame2 moduleName2 = read input
          if ananame==ananame2
            then do
              let modulesToDo3 = reduceDependencies modulesToDo2 [moduleName2]
                  busyWorker2= deleteIndex inputHandle busyWorker
              masterLoop cc (handle:idleWorker) busyWorker2 ananame
                         mainModule modulesToDo3 waitList
            else
             return
              (Just "Received analysis does not match requested analysis type")
    else masterLoop cc idleWorker busyWorker ananame mainModule modulesToDo2
                    waitList
 where dl = debugLevel cc

masterLoop cc (handle:idleWorker) busyWorker ananame mainModule modulesToDo
           (modName:waitList) = do
  debugMessage dl 2 "Master loop: worker available, send task to a worker..."
  let newTask = show (Task ananame modName)
  hPutStrLn handle newTask
  hFlush handle
  debugMessage dl 2 ("Master loop: send message: "++newTask)
  masterLoop cc idleWorker (handle:busyWorker) ananame mainModule
             modulesToDo waitList
 where dl = debugLevel cc

masterLoop cc [] busyWorker ananame mainModule modulesToDo
           waits@(modName:waitList) = do
  debugMessage dl 2 $ "Waiting for worker to analyze modules: "++show waits
  inputHandle <- hWaitForInputs busyWorker waitTime
  if inputHandle < 0
    then return (Just "No input from any worker received")
    else do
      let handle = busyWorker !! inputHandle
      input <- hGetLine handle
      debugMessage dl 2 ("Master loop: got message: "++input)
      let Task _ finishedmodule = read input
          newTask = show (Task ananame modName)
      hPutStrLn handle newTask
      hFlush handle
      debugMessage dl 2 ("Master loop: send message: "++newTask)
      let modulesToDo2 = reduceDependencies modulesToDo [finishedmodule]
      masterLoop cc [] busyWorker ananame mainModule modulesToDo2 waitList
 where dl = debugLevel cc

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ [] = []
deleteIndex n (x:xs) | n==0      = xs
                     | otherwise = x : deleteIndex (n-1) xs

-----------------------------------------------------------------------
