--------------------------------------------------------------------------
--- This is the main module of the analysis server.
--- It provides operations to initialize the server system,
--- start the server on a socket, or use the analysis server
--- by other Curry applications.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version February 2023
--------------------------------------------------------------------------

module CASS.Server
  ( mainServer, initializeAnalysisSystem
  , analyzeModuleAndPrint, analyzeModuleAsText
  , analyzeModuleForBrowser, analyzeFunctionForBrowser
  , analyzeGeneric, analyzeGenericWithDebug, analyzePublic, analyzeInterface
  ) where

import Numeric            ( readNat )
import Data.Char          ( isSpace )
import Control.Monad      ( unless )
import System.CurryPath   ( runModuleAction )
import System.Directory
import System.FilePath
import System.IO
import System.Process     ( system, sleep )
import System.Environment

import Analysis.Logging   ( DLevel, debugMessage )
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
initializeAnalysisSystem = readRCFile >> return ()

--- Start the analysis server on a socket.
mainServer :: CConfig -> Maybe Int -> IO ()
mainServer cconfig mbport = do
  putStrLn "Start Server"
  (port1,socket1) <- maybe listenOnFresh
                           (\p -> listenOn p >>= \s -> return (p,s))
                           mbport
  putStrLn ("Server Port: "++show port1)
  storeServerPortNumber port1
  getDefaultPath cconfig >>= setEnv "CURRYPATH"
  let numworkers = numberOfWorkers cconfig
  if numworkers > 0
   then do
    serveraddress <- getServerAddress
    (workerport,workersocket) <- listenOnFresh
    debugMessage dl 2 ("SERVER: port to workers: "++show workerport)
    handles <- startWorkers cconfig numworkers workersocket serveraddress
                            workerport []
    serverLoop cconfig socket1 handles
    close workersocket
   else
    serverLoop cconfig socket1 []
 where dl = debugLevel cconfig

--- Run the analysis system and print the analysis results in standard textual
--- representation.
--- If the fourth argument is true, all operations are shown,
--- otherwise only the interface operations.
--- The fifth argument is a flag indicating whether the
--- (re-)analysis should be enforced.
analyzeModuleAndPrint :: CConfig -> String -> String -> Bool -> Bool -> IO ()
analyzeModuleAndPrint cconfig ananame mname optall enforce =
  analyzeProgram cconfig ananame enforce AText mname >>=
  putStrLn . formatResult mname "Text" Nothing (not optall)

--- Run the analysis system and show the analysis results in standard textual
--- representation.
--- If the fourth argument is true, all operations are shown,
--- otherwise only the interface operations.
--- The fifth argument is a flag indicating whether the
--- (re-)analysis should be enforced.
--- Note that, before its first use, the analysis system must be initialized
--- by 'initializeAnalysisSystem'.
analyzeModuleAsText :: CConfig -> String -> String -> Bool -> Bool -> IO String
analyzeModuleAsText cconfig ananame mname optall enforce =
  analyzeProgram cconfig ananame enforce AText mname >>=
             return . formatResult mname "Text" Nothing (not optall)

--- Run the analysis system to show the analysis results in the BrowserGUI.
--- The options are read from the rc file.
analyzeModuleForBrowser :: String -> String -> AOutFormat -> IO [(QName,String)]
analyzeModuleForBrowser ananame mname aoutformat = do
  cconfig <- readRCFile
  analyzeProgram cconfig ananame False aoutformat mname >>=
    return . either pinfo2list (const [])
 where
   pinfo2list pinfo = let (pubinfo,privinfo) = progInfo2Lists pinfo
                       in pubinfo++privinfo

--- Run the analysis system to show the analysis result of a single function
--- in the BrowserGUI. The options are read from the rc file.
analyzeFunctionForBrowser :: String -> QName -> AOutFormat -> IO String
analyzeFunctionForBrowser ananame qn@(mname,_) aoutformat = do
  cconfig <- readRCFile
  analyzeProgram cconfig ananame False aoutformat mname >>=
    return . either (maybe "" id . lookupProgInfo qn) (const "")

--- Analyze a given program (i.e., a module possibly prefixed with a
--- directory name) for a given analysis result format.
--- The third argument is a flag indicating whether the
--- (re-)analysis should be enforced.
analyzeProgram :: CConfig -> String -> Bool -> AOutFormat -> String
               -> IO (Either (ProgInfo String) String)
analyzeProgram cconfig ananame enforce aoutformat progname =
  runModuleAction (analyzeModule cconfig ananame enforce aoutformat) progname

--- Analyze a complete module for a given analysis result format.
--- The third argument is a flag indicating whether the
--- (re-)analysis should be enforced.
analyzeModule :: CConfig -> String -> Bool -> AOutFormat -> String
              -> IO (Either (ProgInfo String) String)
analyzeModule cconfig ananame enforce aoutformat modname = do
  getDefaultPath cconfig >>= setEnv "CURRYPATH"
  let numworkers = numberOfWorkers cconfig
  if numworkers > 0
    then do
     serveraddress <- getServerAddress
     (port,socket) <- listenOnFresh
     handles <- startWorkers cconfig numworkers socket serveraddress port []
     result <- runAnalysisWithWorkers cconfig ananame aoutformat enforce
                                      handles modname
     stopWorkers handles
     close socket
     return result
    else runAnalysisWithWorkers cconfig ananame aoutformat enforce [] modname

--- Start the analysis system with a particular analysis.
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one. The options are read from the rc file.
--- Returns either the analysis information or an error message.
analyzeGeneric :: (Read a, Show a)
               => Analysis a -> String -> IO (Either (ProgInfo a) String)
analyzeGeneric = analyzeGenericWithDebug Nothing

--- Start the analysis system with a particular analysis and
--- an optional debug level (first argument).
--- The analysis must be a registered one if workers are used.
--- If it is a combined analysis, the base analysis must be also
--- a registered one. The options are read from the rc file.
--- Returns either the analysis information or an error message.
analyzeGenericWithDebug :: (Read a, Show a) =>
  Maybe Int -> Analysis a -> String -> IO (Either (ProgInfo a) String)
analyzeGenericWithDebug debuglevel analysis moduleName = do
  configrc <- readRCFile
  let cconfig = maybe configrc
                      (\dl -> setDebugLevel dl configrc)
                      debuglevel
  let (mdir,mname) = splitFileName moduleName
  getDefaultPath cconfig >>= setEnv "CURRYPATH"
  curdir <- getCurrentDirectory
  unless (mdir==".") $ setCurrentDirectory mdir
  let numworkers = numberOfWorkers cconfig
  aresult <-
    if numworkers > 0
     then do
      serveraddress <- getServerAddress
      (port,socket) <- listenOnFresh
      handles <- startWorkers cconfig numworkers socket serveraddress port []
      result <- analyzeMain cconfig analysis mname handles False True
      stopWorkers handles
      close socket
      return result
     else
      analyzeMain cconfig analysis mname [] False True
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
startWorkers:: CConfig -> Int -> Socket -> String -> Int -> [Handle]
            -> IO [Handle]
startWorkers cconfig number workersocket serveraddress workerport handles = do
  if number > 0
    then do
      debugMessage dl 4 ("Number:"++(show number))
      let command = unwords [ executableName, " --worker "
                            , serveraddress, show workerport, "&" ]
      debugMessage dl 4 ("system command: " ++ command)
      system command
      debugMessage dl 4 ("Wait for socket accept for client "++show number)
      connection <- waitForSocketAccept workersocket waitTime
      debugMessage dl 4 ("Socket accept for client "++show number)
      case connection of
        Just (_,handle) -> do
          startWorkers cconfig (number-1) workersocket serveraddress workerport
                       (handle:handles)
        Nothing -> do
          putStrLn ("startWorkers: connection error worker "++(show number))
          startWorkers cconfig (number-1) workersocket serveraddress workerport
                       handles
    else return handles
 where dl = debugLevel cconfig

-- stop all workers at server stop
stopWorkers :: [Handle] -> IO ()
stopWorkers [] = return ()
stopWorkers (handle:whandles) = do
  hPutStrLn handle (show StopWorker)
  hClose handle
  stopWorkers whandles

--------------------------------------------------------------------------
-- server loop to answer analysis requests over network
serverLoop :: CConfig -> Socket -> [Handle] -> IO ()
serverLoop cconfig socket1 whandles = do
  --debugMessage 3 "SERVER: serverLoop"
  connection <- waitForSocketAccept socket1 waitTime
  case connection of
    Just (_,handle) -> serverLoopOnHandle cconfig socket1 whandles handle
    Nothing -> do
      putStrLn "serverLoop: connection error: time out in waitForSocketAccept"
      sleep 1
      serverLoop cconfig socket1 whandles

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

serverLoopOnHandle :: CConfig -> Socket -> [Handle] -> Handle -> IO ()
serverLoopOnHandle cconfig socket1 whandles handle = do
  eof <- hIsEOF handle
  if eof
   then do hClose handle
           debugMessage dl 2 "SERVER connection: eof"
           serverLoop cconfig socket1 whandles
   else do
     string <- hGetLineUntilEOF handle
     debugMessage dl 2 ("SERVER got message: "++string)
     let force = False
     case parseServerMessage string of
       ParseError -> do
         sendServerError dl handle ("Illegal message received: "++string)
         serverLoopOnHandle cconfig socket1 whandles handle
       GetAnalysis -> do
         sendServerResult handle showAnalysisNamesAndFormats
         serverLoopOnHandle cconfig socket1 whandles handle
       AnalyzeModule ananame outForm modname public ->
         catch (runAnalysisWithWorkers cconfig ananame AText force whandles
                                       modname >>=
                return . formatResult modname outForm Nothing public >>=
                sendResult)
               sendAnalysisError
       AnalyzeEntity ananame outForm modname functionName ->
         catch (runAnalysisWithWorkers cconfig ananame AText force whandles
                                       modname >>=
                return . formatResult modname outForm
                                      (Just functionName) False >>= sendResult)
               sendAnalysisError
       SetCurryPath path -> do
         setEnv "CURRYPATH" path
         changeWorkerPath path whandles
         sendServerResult handle ""
         serverLoopOnHandle cconfig socket1 whandles handle
       StopServer -> do
         stopWorkers whandles
         sendServerResult handle ""
         hClose handle
         close socket1
         putStrLn "Stop Server"
         removeServerPortNumber
 where
  dl = debugLevel cconfig

  sendResult resultstring = do
    debugMessage dl 4 ("formatted result:\n"++resultstring)
    sendServerResult handle resultstring
    serverLoopOnHandle cconfig socket1 whandles handle

  sendAnalysisError err = do
    sendServerError dl handle ("ERROR in analysis server: "++ show err)
    serverLoopOnHandle cconfig socket1 whandles handle

-- Send a server result in the format "ok <n>\n<result text>" where <n>
-- is the number of lines of the <result text>.
sendServerResult :: Handle -> String -> IO ()
sendServerResult handle resultstring = do
  let resultlines = lines resultstring
  hPutStrLn handle ("ok " ++ show (length resultlines))
  hPutStr handle (unlines resultlines)
  hFlush handle

-- Send a server error in the format "error <error message>\n".
sendServerError :: DLevel -> Handle -> String -> IO ()
sendServerError dl handle errstring = do
  debugMessage dl 1 errstring
  hPutStrLn handle ("error "++errstring)
  hFlush handle

-- Inform the worker threads about a given changed library search path
changeWorkerPath :: String -> [Handle] -> IO ()
changeWorkerPath _ [] = return ()
changeWorkerPath path (handle:whandles) = do
  hPutStrLn handle (show (ChangePath path))
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
