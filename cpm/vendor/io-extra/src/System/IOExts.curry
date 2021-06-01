------------------------------------------------------------------------------
--- Library with some useful extensions to the IO monad.
---
--- @author Michael Hanus
--- @version April 2021
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module System.IOExts
  ( -- execution of shell commands
    execCmd, evalCmd, connectToCommand
    -- file access
  , readCompleteFile,updateFile, exclusiveIO
  ) where

import Control.Monad    ( unless )
#ifdef __KICS2__
#else
import Data.Char        ( isAlphaNum )
import System.Directory ( removeFile )
#endif
import System.IO
import System.Process

--- Executes a command with a new default shell process.
--- The standard I/O streams of the new process (stdin,stdout,stderr)
--- are returned as handles so that they can be explicitly manipulated.
--- They should be closed with `IO.hClose` since they are not
--- closed automatically when the process terminates.
--- @param cmd - the shell command to be executed
--- @return the handles of the input/output/error streams of the new process
execCmd :: String -> IO (Handle, Handle, Handle)
execCmd cmd = prim_execCmd $## cmd

prim_execCmd :: String -> IO (Handle, Handle, Handle)
prim_execCmd external

--- Executes a command with the given arguments as a new default shell process
--- and provides the input via the process' stdin input stream.
--- The exit code of the process and the contents written to the standard
--- I/O streams stdout and stderr are returned.
--- @param cmd   - the shell command to be executed
--- @param args  - the command's arguments
--- @param input - the input to be written to the command's stdin
--- @return the exit code and the contents written to stdout and stderr
evalCmd :: String -> [String] -> String -> IO (Int, String, String)
#ifdef __KICS2__
evalCmd cmd args input = ((prim_evalCmd $## cmd) $## args) $## input

prim_evalCmd :: String -> [String] -> String -> IO (Int, String, String)
prim_evalCmd external
#else
evalCmd cmd args input = do
  pid <- getPID
  let tmpfile = "/tmp/PAKCS_evalCMD"++show pid
  (hi,ho,he) <- execCmd (unwords (map wrapArg (cmd:args)) ++
                         " ; (echo $? > "++tmpfile++")")
  unless (null input) (hPutStrLn hi input)
  hClose hi
  outs <- hGetContents ho
  errs <- hGetContents he
  ecodes <- readCompleteFile tmpfile
  removeFile tmpfile
  return (read ecodes, outs, errs)
 where
  wrapArg str
    | null str         = "''"
    -- goodChar is a pessimistic predicate, such that if an argument is
    -- non-empty and only contains goodChars, then there is no need to
    -- do any quoting or escaping
    | all goodChar str = str
    | otherwise        = '\'' : foldr escape "'" str
      where escape c s
              | c == '\'' = "'\\''" ++ s
              | otherwise = c : s
            goodChar c    = isAlphaNum c || c `elem` "-_.,/"
#endif


--- Executes a command with a new default shell process.
--- The input and output streams of the new process is returned
--- as one handle which is both readable and writable.
--- Thus, writing to the handle produces input to the process and
--- output from the process can be retrieved by reading from this handle.
--- The handle should be closed with `IO.hClose` since they are not
--- closed automatically when the process terminates.
--- @param cmd - the shell command to be executed
--- @return the handle connected to the input/output streams
---         of the new process
connectToCommand :: String -> IO Handle
connectToCommand cmd = prim_connectToCmd $## cmd

prim_connectToCmd :: String -> IO Handle
prim_connectToCmd external


--- An action that reads the complete contents of a file and returns it.
--- This action can be used instead of the (lazy) `readFile`
--- action if the contents of the file might be changed.
--- @param file - the name of the file
--- @return the complete contents of the file
readCompleteFile :: String -> IO String
readCompleteFile file = do
  s <- readFile file
  f s (return s)
 where
   f []     r = r
   f (_:cs) r = f cs r


--- An action that updates the contents of a file.
--- @param f - the function to transform the contents
--- @param file - the name of the file
updateFile :: (String -> String) -> String -> IO ()
updateFile f file = do
  s <- readCompleteFile file
  writeFile file (f s)


--- Forces the exclusive execution of an action via a lock file.
--- For instance, (exclusiveIO "myaction.lock" act) ensures that
--- the action "act" is not executed by two processes on the same
--- system at the same time.
--- @param lockfile - the name of a global lock file
--- @param action - the action to be exclusively executed
--- @return the result of the execution of the action
exclusiveIO :: String -> IO a -> IO a
exclusiveIO lockfile action = do
  system ("lockfile-create --lock-name "++lockfile)
  catch (do actionResult <- action
            deleteLockFile
            return actionResult )
        (\e -> deleteLockFile >> ioError e)
 where
  deleteLockFile = system $ "lockfile-remove --lock-name " ++ lockfile
