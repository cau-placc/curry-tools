------------------------------------------------------------------------
--- Implementation of the analysis computations on the server side
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version December 2018
------------------------------------------------------------------------

-- analysis computations on the server side

module CASS.ServerFunctions where

import FlatCurry.Types   (QName)
import FlatCurry.Goodies (progImports)
import IO(Handle(..),hClose,hFlush,hGetLine,hPutStrLn,hWaitForInput,hWaitForInputs)
import ReadShowTerm(readQTerm,showQTerm)
import System(system,sleep)
import Directory(doesFileExist,getModificationTime)
import Maybe(fromMaybe)
import List(delete)
import Time(ClockTime)
import XML(showXmlDoc,xml)

import Analysis.Logging(debugMessage)
import Analysis.Types
import Analysis.ProgInfo
import CASS.Dependencies
import CASS.Configuration(waitTime)

data WorkerMessage = Task String String | ChangePath String | StopWorker


-- Master loop for communication with workers
-- Argument 1: handles for workers that are currently free
-- Argument 2: handles for workers that are currently busy
-- Argument 3: the analysis name
-- Argument 4: the name of the main module
-- Argument 5: the modules to be analyzed (with their dependencies)
-- Argument 6: names of modules that are ready be to analyzed (since their
--             imports are already analyzed)
-- Result: Nothing (in case of successful work) or (Just <error>)
masterLoop :: [Handle] -> [Handle] -> String -> String
           -> [(String,[String])] -> [String] -> IO (Maybe String)
masterLoop _ [] _ _ [] [] = do
  debugMessage 2 "Master loop: terminated"
  return Nothing

masterLoop _ (b:busyWorker) ananame mainModule [] [] = do
  debugMessage 2 "Master loop: waiting for worker result"
  inputHandle <- hWaitForInputs (b:busyWorker) waitTime 
  if inputHandle/=0
    then return (Just "No input from any worker received")
    else do
      let handle =  b
      input <- hGetLine handle
      debugMessage 2 ("Master loop: got message: "++input)
      let Task ananame2 moduleName2 = readQTerm input
      if ananame==ananame2 && moduleName2==mainModule
        then return Nothing
        else return (Just "Received analysis does not match requested analysis")

masterLoop idleWorker busyWorker ananame mainModule
           modulesToDo@(_:_) [] = do
  debugMessage 3 ("Master loop: modules to do: "++(showQTerm modulesToDo))
  let modulesToDo2 = filter ((not . null) . snd) modulesToDo
      waitList     = map fst (filter (null . snd) modulesToDo)
  if null waitList
    then do
      debugMessage 2 "Master loop: waiting for workers to finish"
      inputHandle <- hWaitForInputs busyWorker waitTime 
      if inputHandle<0
        then return (Just "No input from any worker received")
        else do
          let handle =  busyWorker !! inputHandle
          input <- hGetLine handle
          debugMessage 2 ("Master loop: got message: "++input)
          let Task ananame2 moduleName2 = readQTerm input
          if ananame==ananame2
            then do
              let modulesToDo3 = reduceDependencies modulesToDo2 [moduleName2]
                  busyWorker2= deleteIndex inputHandle busyWorker 
              masterLoop (handle:idleWorker) busyWorker2 ananame
                         mainModule modulesToDo3 waitList  
            else
             return
              (Just "Received analysis does not match requested analysis type")
    else masterLoop idleWorker busyWorker ananame mainModule modulesToDo2
                    waitList

masterLoop (handle:idleWorker) busyWorker ananame mainModule modulesToDo
           (modName:waitList) = do
  debugMessage 2 "Master loop: worker available, send task to a worker..."
  let newTask = showQTerm (Task ananame modName)
  hPutStrLn handle newTask
  hFlush handle
  debugMessage 2 ("Master loop: send message: "++newTask)
  masterLoop idleWorker (handle:busyWorker) ananame mainModule
             modulesToDo waitList

masterLoop [] busyWorker ananame mainModule modulesToDo
           waits@(modName:waitList) = do
  debugMessage 2 $ "Waiting for worker to analyze modules: "++show waits
  inputHandle <- hWaitForInputs busyWorker waitTime 
  if inputHandle<0
    then return (Just "No input from any worker received")
    else do
      let handle = busyWorker !! inputHandle
      input <- hGetLine handle
      debugMessage 2 ("Master loop: got message: "++input)
      let Task _ finishedmodule = readQTerm input
          newTask = showQTerm (Task ananame modName)
      hPutStrLn handle newTask
      hFlush handle
      debugMessage 2 ("Master loop: send message: "++newTask)
      let modulesToDo2 = reduceDependencies modulesToDo [finishedmodule]
      masterLoop [] busyWorker ananame mainModule modulesToDo2 waitList

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ [] = []
deleteIndex n (x:xs) | n==0      = xs
                     | otherwise = x : deleteIndex (n-1) xs

-----------------------------------------------------------------------
