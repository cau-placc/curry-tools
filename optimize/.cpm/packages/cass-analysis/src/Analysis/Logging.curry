--------------------------------------------------------------------------
--- This module provides operation for log messages and setting
--- the log level for the analyses.
---
--- @author Michael Hanus
--- @version April 2021
--------------------------------------------------------------------------

module Analysis.Logging ( DLevel(..), debugMessage, debugString ) where

import Control.Monad

--------------------------------------------------------------------------
--- Debug levels intended as first parameter in debug operations below:
--- 0 : show nothing
--- 1 : show worker activity, e.g., timings
--- 2 : show server communication
--- 3 : ...and show read/store information
--- 4 : ...show also stored/computed analysis data
data DLevel = Quiet | Timing | Communicate | Storage | AllData
 deriving Enum

--- Prints a message line if debugging level is at least n:
debugMessage :: DLevel -> Int -> String -> IO ()
debugMessage dl n message = debugString dl n (message ++ "\n")

--- Prints a string if debugging level (as specified in the Config file)
--- is at least n:
debugString :: DLevel -> Int -> String -> IO ()
debugString dl n message = when (fromEnum dl >= n) $ putStr message

--------------------------------------------------------------------------
