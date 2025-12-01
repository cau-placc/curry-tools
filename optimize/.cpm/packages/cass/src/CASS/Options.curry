--------------------------------------------------------------------------
--- Defining and processing tool options of CASS.
---
--- @author Michael Hanus
--- @version January 2025
--------------------------------------------------------------------------

module CASS.Options where

import Data.Char             ( toLower )
import Numeric               ( readNat )
import System.Console.GetOpt

import CASS.ServerFormats

--------------------------------------------------------------------------
-- Representation of command line options.
data Options = Options
  { optHelp        :: Bool         -- print help?
  , optVerb        :: Int          -- verbosity level
  , optServer      :: Bool         -- start CASS in server mode?
  , optWorker      :: Bool         -- start CASS in worker mode?
  , optPort        :: Int          -- port number (if used in server mode)
  , optAll         :: Bool         -- show analysis results for all operations?
  , optGenerated   :: Bool         -- show results for generated operations?
  , optFormat      :: OutputFormat -- output format
  , optReAna       :: Bool         -- force re-analysis?
  , optStats       :: Bool         -- show statitics at the end?
  , optDelete      :: Bool         -- delete analysis files?
  , optNoCurryInfo :: [String]     -- modules where CurryInfo should not be asked
  , optProp        :: [(String,String)] -- property (of ~/.curryanalsisrc)
                                        -- to be set during this analysis run
  }

-- Default command line options.
defaultOptions :: Options
defaultOptions = Options
  { optHelp        = False
  , optVerb        = -1
  , optServer      = False
  , optWorker      = False
  , optPort        = 0
  , optAll         = False
  , optGenerated   = True
  , optFormat      = FormatText
  , optReAna       = False
  , optStats       = False
  , optDelete      = False
  , optNoCurryInfo = []
  , optProp        = []
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
            "verbosity/debug level:\n0: quiet (default; same as `-q')\n1: show worker activity, e.g., timings\n2: show server communication\n3: ...and show read/store information\n4: ...show also stored/computed analysis data\n(default: see debugLevel in ~/.cassrc)"
  , Option "a" ["all"]
           (NoArg (\opts -> opts { optAll = True }))
           "show analysis results for all operations\n(i.e., also for non-exported operations)"
  , Option "f" ["format"]
           (ReqArg checkFormat "<f>")
           "output format (default: Text):\nText|Short|CurryTerm|JSON|JSONTerm|XML"
  , Option "" ["nogenerated"]
           (NoArg (\opts -> opts { optGenerated = False }))
           "do not show results of generated operations\n(e.g., operations derived for class instances)"
  , Option "r" ["reanalyze"]
           (NoArg (\opts -> opts { optReAna = True }))
           "force re-analysis \n(i.e., ignore old analysis information)"
  , Option "" ["statistics"]
           (NoArg (\opts -> opts { optStats = True }))
           "show statistics about number of modules"
  , Option "" ["curryinfo"]
           (NoArg (\opts -> opts { optProp =
                                    optProp opts ++ [("curryinfo","local")] }))
           "use `curry-info` to import analysis infos"
  , Option "" ["curryinfoweb"]
           (NoArg (\opts -> opts { optProp =
                                    optProp opts ++ [("curryinfo","remote")] }))
           "use `curry-info` web server for analysis infos"
  , Option "" ["nocurryinfo"]
           (ReqArg (\m opts ->
                      opts { optNoCurryInfo = m : optNoCurryInfo opts}) "<m>")
           "do not use `curry-info` for module <m>"
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
           "set property (of ~/.cassrc)\n`name' as `v'"
  ]
 where
  safeReadNat opttrans s opts = case readNat s of
     [(n,"")] -> opttrans n opts
     _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n>=0 && n<5
                       then opts { optVerb = n }
                       else error "Illegal verbosity level (try `-h' for help)"

  checkFormat s opts =
    maybe (error $ "Illegal format value: " ++ s)
          (\f -> opts { optFormat = f })
          (lookup (map toLower s) serverFormatNames)

  checkSetProperty s opts =
    let (key,eqvalue) = break (=='=') s
     in if null eqvalue
         then error "Illegal property setting (try `-h' for help)"
         else opts { optProp = optProp opts ++ [(key,tail eqvalue)] }

--------------------------------------------------------------------------
