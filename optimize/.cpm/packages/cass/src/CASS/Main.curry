--------------------------------------------------------------------------
--- This is the main module to start the executable of the analysis system.
---
--- @author Michael Hanus
--- @version December 2018
--------------------------------------------------------------------------

module CASS.Main ( main ) where

import Data.Char             ( toLower )
import Data.List             ( isPrefixOf, sort )
import Control.Monad         ( when, unless )
import System.FilePath       ( (</>), (<.>) )
import System.Process        ( exitWith )
import System.Environment    ( getArgs )
import System.Console.GetOpt
import Numeric               ( readNat )
import ReadShowTerm          ( readQTerm )

import Analysis.Files     ( deleteAllAnalysisFiles )
import Analysis.Logging   ( debugMessage )
import CASS.Doc           ( getAnalysisDoc )
import CASS.Server
import CASS.Configuration
import CASS.Registry
import CASS.Worker        ( startWorker )
import System.CurryPath   ( stripCurrySuffix )

--- Main function to start the analysis system.
--- With option -s or --server, the server is started on a socket.
--- Otherwise, it is started in batch mode to analyze a single module.
main :: IO ()
main = do
  argv <- getArgs
  let (funopts, args, opterrors) = getOpt Permute options argv
  let opts = foldl (flip id) defaultOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> putStr usageText >> exitWith 1)
  initializeAnalysisSystem
  when (optHelp opts) (printHelp args >> exitWith 1)
  when (optDelete opts) (deleteFiles args)
  when ((optServer opts && not (null args)) ||
        (not (optServer opts) && length args /= 2))
       (error "Illegal arguments (try `-h' for help)" >> exitWith 1)
  when (optWorker opts && length args /= 2)
       (error "Illegal arguments (try `-h' for help)" >> exitWith 1)
  mapM_ (\ (k,v) -> updateCurrentProperty k v) (optProp opts)
  let verb = optVerb opts
  when (verb >= 0) (updateCurrentProperty "debugLevel" (show verb))
  debugMessage 1 systemBanner
  if optServer opts
   then mainServer (let p = optPort opts in if p == 0 then Nothing else Just p)
   else
     if optWorker opts
       then startWorker (head args) (readQTerm (args!!1))
       else do
         let [ananame,mname] = args
         fullananame <- checkAnalysisName ananame
         putStrLn $ "Computing results for analysis `" ++ fullananame ++ "'"
         analyzeModuleAsText fullananame (stripCurrySuffix mname)
                             (optAll opts) (optReAna opts) >>= putStrLn
 where
  deleteFiles args = case args of
    [aname] -> do fullaname <- checkAnalysisName aname
                  putStrLn $ "Deleting files for analysis `" ++ fullaname ++ "'"
                  deleteAllAnalysisFiles fullaname
                  exitWith 0
    [] -> error "Missing analysis name!"
    _  -> error "Too many arguments (only analysis name should be given)!"

-- Checks whether a given analysis name is a unique abbreviation
-- of a registered analysis name and return the registered name.
-- Otherwise, raise an error.
checkAnalysisName :: String -> IO String
checkAnalysisName aname = case matchedNames of
  []       -> error $ "Unknown analysis name `"++ aname ++ "' " ++ tryCmt
  [raname] -> return raname
  (_:_:_)  -> error $ "Analysis name `"++ aname ++ "' not unique " ++ tryCmt ++
                      ":\nPossible names are: " ++ unwords matchedNames
 where
  matchedNames = filter (isPrefixOf (map toLower aname) . map toLower)
                        registeredAnalysisNames
  tryCmt       = "(try `-h' for help)"

--------------------------------------------------------------------------
-- Representation of command line options.
data Options = Options
  { optHelp    :: Bool     -- print help?
  , optVerb    :: Int      -- verbosity level
  , optServer  :: Bool     -- start CASS in server mode?
  , optWorker  :: Bool     -- start CASS in worker mode?
  , optPort    :: Int      -- port number (if used in server mode)
  , optAll     :: Bool     -- show analysis results for all operations?
  , optReAna   :: Bool     -- force re-analysis?
  , optDelete  :: Bool     -- delete analysis files?
  , optProp    :: [(String,String)] -- property (of ~/.curryanalsisrc) to be set
  }

-- Default command line options.
defaultOptions :: Options
defaultOptions = Options
  { optHelp    = False
  , optVerb    = -1
  , optServer  = False
  , optWorker  = False
  , optPort    = 0
  , optAll     = False
  , optReAna   = False
  , optDelete  = False
  , optProp    = []
  }

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]  (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"] (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output)"
  , Option "v" ["verbosity"]
            (ReqArg (safeReadNat checkVerb) "<n>")
            "verbosity/debug level:\n0: quiet (same as `-q')\n1: show worker activity, e.g., timings\n2: show server communication\n3: ...and show read/store information\n4: ...show also stored/computed analysis data\n(default: see debugLevel in ~/.curryanalysisrc)"
  , Option "a" ["all"]
           (NoArg (\opts -> opts { optAll = True }))
           "show-analysis results for all operations\n(i.e., also for non-exported operations)"
  , Option "r" ["reanalyze"]
           (NoArg (\opts -> opts { optReAna = True }))
           "force re-analysis \n(i.e., ignore old analysis information)"
  , Option "d" ["delete"]
           (NoArg (\opts -> opts { optDelete = True }))
           "delete existing analysis results"
  , Option "s" ["server"]
           (NoArg (\opts -> opts { optServer = True }))
           "start analysis system in server mode"
  , Option "w" ["worker"]
           (NoArg (\opts -> opts { optWorker = True }))
           "start analysis system in worker mode"
  , Option "p" ["port"]
           (ReqArg (safeReadNat (\n opts -> opts { optPort = n })) "<n>")
           "port number for communication\n(only for server mode;\n if omitted, a free port number is selected)"
  , Option "D" []
            (ReqArg checkSetProperty "name=v")
           "set property (of ~/.curryanalysisrc)\n`name' as `v'"
  ]
 where
  safeReadNat opttrans s opts = case readNat s of
     [(n,"")] -> opttrans n opts
     _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n>=0 && n<5
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

  checkSetProperty s opts =
    let (key,eqvalue) = break (=='=') s
     in if null eqvalue
         then error "Illegal property setting (try `-h' for help)"
         else opts { optProp = optProp opts ++ [(key,tail eqvalue)] }


--------------------------------------------------------------------------
-- Printing help:
printHelp :: [String] -> IO ()
printHelp args =
  if null args
   then putStrLn $ systemBanner ++ "\n" ++ usageText
   else do aname <- checkAnalysisName (head args)
           getAnalysisDoc aname >>=
             maybe (putStrLn $
                      "Sorry, no documentation for analysis `" ++ aname ++ "'")
                   putStrLn

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: curry analyze <options> <analysis name> <module name>\n" ++
             "   or: curry analyze <options> [-s|--server]\n" ++
             "   or: curry analyze [-w|--worker] <host> <port>\n")
            options ++
  unlines ("" : "Registered analyses names:" :
           "(use option `-h <analysis name>' for more documentation)" :
           "" : map showAnaInfo (sort registeredAnalysisInfos))
 where
  maxName = foldr1 max (map (length . fst) registeredAnalysisInfos) + 1
  showAnaInfo (n,t) = n ++ take (maxName - length n) (repeat ' ') ++ ": " ++ t

--------------------------------------------------------------------------
