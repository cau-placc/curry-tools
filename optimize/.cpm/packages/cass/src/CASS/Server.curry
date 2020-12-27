--------------------------------------------------------------------------
--- This is the main module of the analysis server.
--- It provides operations to initialize the server system,
--- start the server on a socket, or use the analysis server
--- by other Curry applications.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version December 2020
--------------------------------------------------------------------------

module CASS.Server
  (mainServer, initializeAnalysisSystem, analyzeModuleAsText
  , analyzeModuleForBrowser, analyzeFunctionForBrowser
  , analyzeGeneric, analyzePublic, analyzeInterface
  ) where

import Numeric            ( readNat )
import ReadShowTerm       ( readQTerm, showQTerm )
import Data.Char          ( isSpace )
import Control.Monad      ( unless )
import System.CurryPath   ( runModuleAction )
import System.Directory
import System.FilePath
import System.IO
import System.Process     ( system, sleep )
import System.Environment

import Analysis.Logging   ( debugMessage )
import Analysis.ProgInfo
import Analysis.Types     ( Analysis, AOutFormat(..) )
import FlatCurry.Types    ( QName )
import Network.Socket     ( Socket(..), listenOn, listenOnFresh
                          , close, waitForSocketAccept )

import CASS.Configuration
import CASS.Registry
import CASS.ServerFormats
import CASS.ServerFunctions(WorkerMessage(..))

-- Messages to communicate with the analysis server from external programs.
data AnalysisServerMessage =
    GetAnalysis
  | AnalyzeModule    String String String Bool
  | AnalyzeEntity  String String String String
  | StopServer
  | SetCurryPath String
  | ParseError

--- Initializations to be done when the system is started.
initializeAnalysisSystem :: IO ()
initializeAnalysisSystem = updateRCFile

--- Start the analysis server on a socket.
mainServer :: Maybe Int -> IO ()
mainServer mbport = do
  putStrLn "Start Server"
  (port1,socket1) <- maybe listenOnFresh
                           (\p -> listenOn p >>= \s -> return (p,s))
                           mbport
  putStrLn ("Server Port: "++show port1)
  storeServerPortNumber port1
  getDefaultPath >>= setEnv "CURRYPATH"
  numworkers <- numberOfWorkers
  if numworkers>0
   then do
    serveraddress <- getServerAddress
    (workerport,workersocket) <- listenOnFresh
    debugMessage 2 ("SERVER: port to workers: "++show workerport)
    handles <- startWorkers numworkers workersocket serveraddress workerport []
    serverLoop socket1 handles
    close workersocket
   else
    serverLoop socket1 []


--- Run the analysis system and show the analysis results in standard textual
--- representation.
--- If the third argument is true, all operations are shown,
--- otherwise only the interface operations.
--- The fourth argument is a flag indicating whether the
--- (re-)analysis should be enforced.
--- Note that, before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeModuleAsText :: String -> String -> Bool -> Bool -> IO String
analyzeModuleAsText ananame mname optall enforce =
  analyzeProgram ananame enforce AText mname >>=
  return . formatResult mname "Text" Nothing (not optall)

--- Run the analysis system to show the analysis results in the BrowserGUI.
--- Note that, before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeModuleForBrowser :: String -> String -> AOutFormat -> IO [(QName,String)]
analyzeModuleForBrowser ananame mname aoutformat =
  analyzeProgram ananame False aoutformat mname >>=
    return . either pinfo2list (const [])
 where
   pinfo2list pinfo = let (pubinfo,privinfo) = progInfo2Lists pinfo
                       in pubinfo++privinfo

--- Run the analysis system to show the analysis result of a single function
--- in the BrowserGUI.
--- Note that before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeFunctionForBrowser :: String -> QName -> AOutFormat -> IO String
analyzeFunctionForBrowser ananame qn@(mname,_) aoutformat = do
  analyzeProgram ananame False aoutformat mname >>=
    return . either (maybe "" id . lookupProgInfo qn) (const "")

--- Analyze a given program (i.e., a module possibly prefixed with a
--- directory name) for a given analysis result format.
--- The third argument is a flag indicating whether the
--- (re-)analysis should be enforced.
--- Note that before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeProgram :: String -> Bool -> AOutFormat -> String
               -> IO (Either (ProgInfo String) String)
analyzeProgram ananame enforce aoutformat progname =
  runModuleAction (analyzeModule ananame enforce aoutformat) progname

--- Analyze a complete module for a given analysis result format.
--- The second argument is a flag indicating whether the
--- (re-)analysis should be enforced.
--- Note that before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeModule :: String -> Bool -> AOutFormat -> String
              -> IO (Either (ProgInfo String) String)
analyzeModule ananame enforce aoutformat modname = do
  getDefaultPath >>= setEnv "CURRYPATH"
  numworkers <- numberOfWorkers
  if numworkers>0
    then do
     serveraddress <- getServerAddress
     (port,socket) <- listenOnFresh
     handles <- startWorkers numworkers socket serveraddress port []
     result <- runAnalysisWithWorkers ananame aoutformat enforce handles modname
     stopWorkers handles
     close socket
     return result
    else runAnalysisWithWorkers ananame aoutformat enforce [] modname

--- Start the analysis system with a particular analysis.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one.
--- Returns either the analysis information or an error message.
analyzeGeneric :: (Read a, Show a)
               => Analysis a -> String -> IO (Either (ProgInfo a) String)
analyzeGeneric analysis moduleName = do
  initializeAnalysisSystem
  let (mdir,mname) = splitFileName moduleName
  getDefaultPath >>= setEnv "CURRYPATH"
  curdir <- getCurrentDirectory
  unless (mdir==".") $ setCurrentDirectory mdir
  numworkers <- numberOfWorkers
  aresult <-
    if numworkers>0
     then do
      serveraddress <- getServerAddress
      (port,socket) <- listenOnFresh
      handles <- startWorkers numworkers socket serveraddress port []
      result <- analyzeMain analysis mname handles False True
      stopWorkers handles
      close socket
      return result
     else
      analyzeMain analysis mname [] False True
  setCurrentDirectory curdir
  return aresult

--- Start the analysis system with a given analysis to compute properties
--- of a module interface.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one.
--- Returns either the analysis information or an error message.
analyzePublic :: (Read a, Show a)
              => Analysis a -> String -> IO (Either (ProgInfo a) String)
analyzePublic analysis moduleName =
  analyzeGeneric analysis moduleName
  >>= return . either (Left . publicProgInfo) Right

--- Start the analysis system with a given analysis to compute properties
--- of a module interface.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one.
analyzeInterface :: (Read a, Show a)
                 => Analysis a -> String -> IO (Either [(QName,a)] String)
analyzeInterface analysis moduleName =
  analyzeGeneric analysis moduleName
  >>= return . either (Left . publicListFromProgInfo) Right

--------------------------------------------------------------------------
-- start a number of workers at server start
startWorkers:: Int -> Socket -> String -> Int -> [Handle] -> IO [Handle]
startWorkers number workersocket serveraddress workerport handles = do
  if number>0
    then do
      debugMessage 4 ("Number:"++(show number))
      let command = unwords [ executableName, " --worker "
                            , serveraddress, show workerport, "&" ]
      debugMessage 4 ("system command: "++command)
      system command
      debugMessage 4 ("Wait for socket accept for client "++show number)
      connection <- waitForSocketAccept workersocket waitTime
      debugMessage 4 ("Socket accept for client "++show number)
      case connection of
        Just (_,handle) -> do
          startWorkers (number-1) workersocket serveraddress workerport
                       (handle:handles)
        Nothing -> do
          putStrLn ("startWorkers: connection error worker "++(show number))
          startWorkers (number-1) workersocket serveraddress workerport handles
    else return handles

-- stop all workers at server stop
stopWorkers :: [Handle] -> IO ()
stopWorkers [] = return ()
stopWorkers (handle:whandles) = do
  hPutStrLn handle (showQTerm StopWorker)
  hClose handle
  stopWorkers whandles

--------------------------------------------------------------------------
-- server loop to answer analysis requests over network
serverLoop :: Socket -> [Handle] -> IO ()
serverLoop socket1 whandles = do
  --debugMessage 3 "SERVER: serverLoop"
  connection <- waitForSocketAccept socket1 waitTime
  case connection of
    Just (_,handle) -> serverLoopOnHandle socket1 whandles handle
    Nothing -> do
      putStrLn "serverLoop: connection error: time out in waitForSocketAccept"
      sleep 1
      serverLoop socket1 whandles

--- Reads a line from an input handle and returns it.
hGetLineUntilEOF  :: Handle -> IO String
hGetLineUntilEOF h = do
  eof <- hIsEOF h
  if eof
   then return ""
   else do c <- hGetChar h
           if c=='\n' then return ""
                      else do cs <- hGetLineUntilEOF h
                              return (c:cs)

serverLoopOnHandle :: Socket -> [Handle] -> Handle -> IO ()
serverLoopOnHandle socket1 whandles handle = do
  eof <- hIsEOF handle
  if eof
   then do hClose handle
           debugMessage 2 "SERVER connection: eof"
           serverLoop socket1 whandles
   else do
     string <- hGetLineUntilEOF handle
     debugMessage 2 ("SERVER got message: "++string)
     let force = False
     case parseServerMessage string of
       ParseError -> do
         sendServerError handle ("Illegal message received: "++string)
         serverLoopOnHandle socket1 whandles handle
       GetAnalysis -> do
         sendServerResult handle showAnalysisNamesAndFormats
         serverLoopOnHandle socket1 whandles handle
       AnalyzeModule ananame outForm modname public ->
         catch (runAnalysisWithWorkers ananame AText force whandles modname >>=
                return . formatResult modname outForm Nothing public >>=
                sendResult)
               sendAnalysisError
       AnalyzeEntity ananame outForm modname functionName ->
         catch (runAnalysisWithWorkers ananame AText force whandles modname >>=
                return . formatResult modname outForm
                                      (Just functionName) False >>= sendResult)
               sendAnalysisError
       SetCurryPath path -> do
         setEnv "CURRYPATH" path
         changeWorkerPath path whandles
         sendServerResult handle ""
         serverLoopOnHandle socket1 whandles handle
       StopServer -> do
         stopWorkers whandles
         sendServerResult handle ""
         hClose handle
         close socket1
         putStrLn "Stop Server"
         removeServerPortNumber
 where
  sendResult resultstring = do
    debugMessage 4 ("formatted result:\n"++resultstring)
    sendServerResult handle resultstring
    serverLoopOnHandle socket1 whandles handle

  sendAnalysisError err = do
    sendServerError handle ("ERROR in analysis server: "++ show err)
    serverLoopOnHandle socket1 whandles handle

-- Send a server result in the format "ok <n>\n<result text>" where <n>
-- is the number of lines of the <result text>.
sendServerResult :: Handle -> String -> IO ()
sendServerResult handle resultstring = do
  let resultlines = lines resultstring
  hPutStrLn handle ("ok " ++ show (length resultlines))
  hPutStr handle (unlines resultlines)
  hFlush handle

-- Send a server error in the format "error <error message>\n".
sendServerError :: Handle -> String -> IO ()
sendServerError handle errstring = do
  debugMessage 1 errstring
  hPutStrLn handle ("error "++errstring)
  hFlush handle

-- Inform the worker threads about a given changed library search path
changeWorkerPath :: String -> [Handle] -> IO ()
changeWorkerPath _ [] = return ()
changeWorkerPath path (handle:whandles) = do
  hPutStrLn handle (showQTerm (ChangePath path))
  changeWorkerPath path whandles

-- parse incoming message for type of request
parseServerMessage :: String -> AnalysisServerMessage
parseServerMessage message = case words message of
  [] -> ParseError
  w:ws -> case w of
    "GetAnalysis" -> GetAnalysis
    "AnalyzeModule" -> case ws of
      s1:s2:s3:[] -> checkFormat s2 $ AnalyzeModule s1 s2 s3 False
      _ -> ParseError
    "AnalyzeInterface" -> case ws of
      s1:s2:s3:[] -> checkFormat s2 $ AnalyzeModule s1 s2 s3 True
      _ -> ParseError
    "AnalyzeFunction" -> case ws of
      s1:s2:s3:s4:[] -> checkFormat s2 $ AnalyzeEntity s1 s2 s3 s4
      _ -> ParseError
    "AnalyzeTypeConstructor" -> case ws of
      s1:s2:s3:s4:[] -> checkFormat s2 $ AnalyzeEntity s1 s2 s3 s4
      _ -> ParseError
    "AnalyzeDataConstructor" -> case ws of
      s1:s2:s3:s4:[] -> checkFormat s2 $ AnalyzeEntity s1 s2 s3 s4
      _ -> ParseError
    "SetCurryPath" -> case ws of
      s:[] -> SetCurryPath s
      _ -> ParseError
    "StopServer" -> StopServer
    _ -> ParseError
 where
  checkFormat fmt msg = if fmt `elem` serverFormats then msg else ParseError

--- Show all analysis names and formats.
showAnalysisNamesAndFormats :: String
showAnalysisNamesAndFormats =
  unlines (concatMap (\an -> map ((an++" ")++) serverFormats)
                     registeredAnalysisNames)
