------------------------------------------------------------------------
--- Implementation of a worker client to analyze a module
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version April 2021
------------------------------------------------------------------------

module CASS.Worker ( startWorker ) where

import System.IO            ( Handle, hClose, hFlush, hWaitForInput
                            , hPutStrLn, hGetLine )
import System.Environment   ( getArgs, setEnv )

import Analysis.Logging     ( debugMessage )
import Network.Socket       ( connectToSocket )

import CASS.Configuration   ( CConfig, debugLevel, waitTime, getDefaultPath )
import CASS.Registry        ( lookupRegAnaWorker )
import CASS.ServerFunctions ( WorkerMessage(..) )

startWorker :: CConfig -> String -> Int -> IO ()
startWorker cconfig host port = do
  debugMessage dl 2 ("start analysis worker on port " ++ show port)
  getDefaultPath cconfig >>= setEnv "CURRYPATH"
  handle <- connectToSocket host port
  worker cconfig handle
 where dl = debugLevel cconfig

-- communication loop
worker :: CConfig -> Handle -> IO ()
worker cconfig handle = do
  gotInput <- hWaitForInput handle waitTime
  if gotInput
    then do
       input <- hGetLine handle
       debugMessage dl 3 ("input: "++input)
       case read input of
         Task ananame moduleName -> do
           debugMessage dl 1 ("Start task: "++ananame++" for "++moduleName)
           -- Run the analysis worker for the given analysis and module:
           (lookupRegAnaWorker ananame) cconfig [moduleName]
           debugMessage dl 1 ("Finished task: "++ananame++" for "++moduleName)
           debugMessage dl 3 ("Output: "++input)
           hPutStrLn handle input
           hFlush handle
           worker cconfig handle
         ChangePath path -> do
           setEnv "CURRYPATH" path
           worker cconfig handle
         StopWorker -> do
           debugMessage dl 2 "Stop worker"
           hClose handle
           return ()
    else return ()
 where dl = debugLevel cconfig
