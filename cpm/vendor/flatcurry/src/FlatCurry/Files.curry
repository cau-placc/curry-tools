------------------------------------------------------------------------------
--- This library defines I/O actions to read Curry programs and
--- transform them into the FlatCurry representation.
---
--- @author Michael Hanus, Finn Teegen
--- @version November 2023
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module FlatCurry.Files where

import Control.Monad       ( unless )

import System.Directory    ( doesFileExist, getFileWithSuffix
                           , findFileWithSuffix )
import System.FilePath     ( takeFileName, (</>), (<.>))
import System.CurryPath    ( inCurrySubdir, isValidModuleName
                           , stripCurrySuffix, modNameToPath
                           , lookupModuleSourceInLoadPath, getLoadPathForModule
                           )
import System.FrontendExec ( FrontendParams(..), FrontendTarget (..)
                           , defaultParams, setQuiet, callFrontendWithParams
                           )
import ReadShowTerm        ( readUnqualifiedTerm, showTerm )


import FlatCurry.Types

--- I/O action which parses a Curry module and returns the corresponding
--- FlatCurry program.
--- The argument is the module name (without suffix ".curry"
--- or ".lcurry") and the result is a FlatCurry term representing this
--- module.
---
--- If one wants to parse a Curry module in another directory,
--- e.g., the file `examples/Mod.curry`, one can use the operation
--- `runModuleAction` from module `System.CurryPath` of package `currypath`
--- to transform this I/O action so that it switches into the directory
--- before reading:
---
---     > runModuleAction readFlatCurry examples/Mod.curry
---
readFlatCurry :: String -> IO Prog
readFlatCurry modname =
  readFlatCurryWithParseOptions modname (setQuiet True defaultParams)

--- I/O action which parses a Curry module
--- with respect to some parser options and returns the
--- corresponding FlatCurry program.
--- The argument is the module name (without suffix ".curry"
--- or ".lcurry") and the result is a FlatCurry term representing this
--- module.
---
--- @param modname - the module name (without suffix ".curry")
--- @param options - parameters passed to the front end
readFlatCurryWithParseOptions :: String -> FrontendParams -> IO Prog
readFlatCurryWithParseOptions modname options = do
  unless (isValidModuleName modname) $ putStrLn $
    "WARNING: '" ++ modname ++ "' is not a valid module name!"
  mbsrc <- lookupModuleSourceInLoadPath modname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule modname
      filename <- getFileWithSuffix 
                     (flatCurryFileName (takeFileName modname)) [""]
                     loadpath
      readFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams FCY options modname
      let fcyfile = dir </> outdir options </>
                    modNameToPath (takeFileName modname) <.> "fcy"
      readFlatCurryFile fcyfile

--- Transforms a name of a Curry module (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding FlatCurry program.
flatCurryFileName :: String -> String
flatCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "fcy"

--- Transforms a name of a Curry module (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding FlatCurry program.
flatCurryIntName :: String -> String
flatCurryIntName prog = inCurrySubdir (stripCurrySuffix prog) <.> "fint"

--- I/O action which reads a FlatCurry program from a file in `.fcy` format.
--- In contrast to `readFlatCurry`, this action does not parse
--- a source program. Thus, the argument must be the name of an existing
--- file (with suffix `.fcy`) containing a FlatCurry program in `.fcy`
--- format and the result is a FlatCurry term representing this program.
readFlatCurryFile :: String -> IO Prog
readFlatCurryFile filename = do
  exfcy <- doesFileExist filename
  if exfcy
    then readExistingFCY filename
    else do let subdirfilename = inCurrySubdir filename
            exdirfcy <- doesFileExist subdirfilename
            if exdirfcy
              then readExistingFCY subdirfilename
              else error ("EXISTENCE ERROR: FlatCurry file '" ++ filename ++
                          "' does not exist")
 where
  readExistingFCY fname = do
    filecontents <- readFile fname
#ifdef  __KMCC__
    return (read filecontents)
#else
    return (readUnqualifiedTerm ["FlatCurry.Types","Prelude"] filecontents)
#endif

--- I/O action which returns the interface of a Curry module, i.e.,
--- a FlatCurry program containing only "Public" entities and function
--- definitions without rules (i.e., external functions).
--- The argument is the module name (without suffix ".curry"
--- or ".lcurry") and the result is a FlatCurry term representing the
--- interface of this module.
---
--- If one wants to parse a Curry module in another directory,
--- e.g., the file `examples/Mod.curry`, one can use the operation
--- `runModuleAction` from module `System.CurryPath` of package `currypath`
--- to transform this I/O action so that it switches into the directory
--- before reading:
---
---     > runModuleAction readFlatCurryInt "examples/Mod.curry"
---
readFlatCurryInt :: String -> IO Prog
readFlatCurryInt modname = do
  readFlatCurryIntWithParseOptions modname (setQuiet True defaultParams)

--- I/O action which parses a Curry module
--- with respect to some parser options and returns the FlatCurry
--- interface of this program, i.e.,
--- a FlatCurry program containing only "Public" entities and function
--- definitions without rules (i.e., external functions).
--- The argument is the module name without suffix ".curry"
--- (or ".lcurry") and the result is a FlatCurry term representing the
--- interface of this module.
readFlatCurryIntWithParseOptions :: String -> FrontendParams -> IO Prog
readFlatCurryIntWithParseOptions modname options = do
  unless (isValidModuleName modname) $ putStrLn $
    "WARNING: '" ++ modname ++ "' is not a valid module name!"
  mbsrc <- lookupModuleSourceInLoadPath modname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule modname
      filename <- getFileWithSuffix
                    (flatCurryIntName (takeFileName modname)) [""]
                    loadpath
      readFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams FINT options modname
      let fintfile = dir </> outdir options </>
                     modNameToPath (takeFileName modname) <.> "fint"
      readFlatCurryFile fintfile

--- Writes a FlatCurry program into a file in `.fcy` format.
--- The file is written in the standard location for intermediate files,
--- i.e., in the 'flatCurryFileName' relative to the directory of the
--- Curry source program (which must exist!).
writeFlatCurry :: Prog -> IO ()
writeFlatCurry prog@(Prog mname _ _ _ _) = do
  mbsrc <- lookupModuleSourceInLoadPath mname
  case mbsrc of
    Nothing      -> error $ "Curry source file for module '" ++ mname ++
                            "' not found!"
    Just (dir,_) -> writeFlatCurryFile (flatCurryFileName (dir </> mname)) prog

--- Writes a FlatCurry program into a file in `.fcy` format.
--- The first argument must be the name of the target file
--- (usually with suffix `.fcy`).
writeFlatCurryFile :: String -> Prog -> IO ()
writeFlatCurryFile file prog =
#ifdef  __KMCC__
  writeFile file (show prog)
#else
  writeFile file (showTerm prog)
#endif

--- Writes a FlatCurry program into a file in `.fcy` format.
--- The first argument must be the name of the target file
--- (usually with suffix `.fcy`).
writeFCY :: String -> Prog -> IO ()
writeFCY = writeFlatCurryFile

--- Returns the name of the FlatCurry file of a module in the load path,
--- if this file exists.
lookupFlatCurryFileInLoadPath :: String -> IO (Maybe String)
lookupFlatCurryFileInLoadPath modname =
  getLoadPathForModule modname >>=
  findFileWithSuffix (flatCurryFileName modname) [""]

--- Returns the name of the FlatCurry file of a module in the load path,
--- if this file exists.
getFlatCurryFileInLoadPath :: String -> IO String
getFlatCurryFileInLoadPath modname =
  getLoadPathForModule modname >>=
  getFileWithSuffix (flatCurryFileName modname) [""]
