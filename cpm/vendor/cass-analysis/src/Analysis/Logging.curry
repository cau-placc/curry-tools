--------------------------------------------------------------------------
--- This module provides operation for log messages and setting
--- the log level for the analyses.
---
--- @author Michael Hanus
--- @version March 2017
--------------------------------------------------------------------------

module Analysis.Logging
 ( getDebugLevel, setDebugLevel, debugMessage, debugString
 ) where

import Control.Monad
import Global

--------------------------------------------------------------------------
--- Global variable to store the debug level.
--- Debug levels:
--- 0 : show nothing
--- 1 : show worker activity, e.g., timings
--- 2 : show server communication
--- 3 : ...and show read/store information
--- 4 : ...show also stored/computed analysis data
debugLevel :: Global Int
debugLevel = global 1 Temporary

--- Gets the current debug level.
getDebugLevel :: IO Int
getDebugLevel = readGlobal debugLevel

--- Sets the debug level to a new value.
setDebugLevel :: Int -> IO ()
setDebugLevel l = writeGlobal debugLevel l

--------------------------------------------------------------------------

--- Prints a message line if debugging level is at least n:
debugMessage :: Int -> String -> IO ()
debugMessage n message = debugString n (message++"\n")

--- Prints a string if debugging level (as specified in the Config file)
--- is at least n:
debugString :: Int -> String -> IO ()
debugString n message = do
  dl <- getDebugLevel
  when (dl>=n) $ putStr message

--------------------------------------------------------------------------
