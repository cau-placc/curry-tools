------------------------------------------------------------------------------
--- A library to support global entities in Curry programs.
--- A global entity has a name declared as a top-level entity.
--- Its value can be accessed and modified by IO actions.
--- Global entities can be declared as persistent so that
--- their values are stored across different program executions
--- or temporary so that they will be stored only in memory.
---
--- Currently, it is still experimental so that its interface might
--- be slightly changed in the future.
---
--- A temporary global entity `gt` is a top-level constant of type
--- `GlobalT t`. If `v` is an initial value `v` of type `t`,
--- where the type `t` does not contain type variables or type class
--- contraints, the temporary global entity should be declared in
--- a module `Mod` by:
---
---     gt :: GlobalT t
---     gt = globalT "Mod.gt" v
---
--- The first argument is the qualified name of this program entity
--- and used as a unique name for this global value.
---
--- Similarly, a persistent global entity `gp` with an initial value `v`
--- of type `t` could be declared by:
---
---     gt :: GlobalP t
---     gt = globalPersistent f v
---
--- where the type `t` must not contain type variables and support
--- `Read` and `Show` instances. `f` is the file name
--- where the global value is persistently stored
--- (the file is created and initialized with `v` if it does not exist).
---
--- @author Michael Hanus
--- @version June 2021
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Data.Global
  ( GlobalT, globalT, readGlobalT, writeGlobalT
  , GlobalP, globalP, globalPersistent, readGlobalP, safeReadGlobalP, writeGlobalP
  ) where

import Control.Monad    ( unless )
import System.Directory ( doesFileExist )
import System.IO
import System.IO.Unsafe ( unsafePerformIO )
import System.Process   ( system )

------------------------------------------------------------------------------
-- Implementation of temporary global entities.
-- The implementation requires a specific compiler feature
-- to to translate top-level entities of type `Data.Global.GlobalT`
-- in a specific way, i.e., as constants rather than operations.

--- The abstract type of a temporary global entity.
#ifdef __KICS2__
external data GlobalT _
#else
data GlobalT _ = GlobalT String
#endif

--- `globalT` is used only to declare a temporary global value
--- as a top-level entity. It should not be used elsewhere.
--- The first argument is the unique name of the temporary global entity.
--- It should be the qualified name of the corresponding program entity.
--- The second argument is the initial value which will be evaluated
--- to a ground normal form when the global entity is used for the first time.
globalT :: String -> a -> GlobalT a
globalT n v = (prim_globalT $## n) v

prim_globalT :: String -> a -> GlobalT a
prim_globalT external

--- Reads the current value of a temporary global entity.
readGlobalT :: GlobalT a -> IO a
readGlobalT g = prim_readGlobalT $# g

prim_readGlobalT :: GlobalT a -> IO a
prim_readGlobalT external

--- Updates the value of a temporary global entity.
--- The new value is evaluated to a ground normal form before updating
--- the entity.
writeGlobalT :: GlobalT a -> a -> IO ()
writeGlobalT g v = (prim_writeGlobalT $# g) $## v

prim_writeGlobalT :: GlobalT a -> a -> IO ()
prim_writeGlobalT external

------------------------------------------------------------------------------
-- Implementation of persistent global entities.

--- The abstract type of a persistent global entity.
data GlobalP _ = GlobalP String

--- `globalP` is used only to declare a persistent global value
--- as a top-level entity. It should not be used elsewhere.
--- The first argument is the file name where the global value
--- is persistently stored. The file is created and initialized
--- with the second argument if it does not exist.
globalP :: (Read a, Show a) => String -> a -> GlobalP a
globalP f v = unsafePerformIO $ do
  exf <- doesFileExist f
  unless exf $ writeGlobalP (GlobalP f) v
  return $ GlobalP f

--- `globalPersistent` is used only to declare a persistent global value
--- as a top-level entity. It should not be used elsewhere.
--- The first argument is the file name where the global value
--- is persistently stored. The file is created and initialized
--- with the second argument if it does not exist.
globalPersistent :: (Read a, Show a) => String -> a -> GlobalP a
globalPersistent = globalP

--- Reads the current value of a persistent global entity.
readGlobalP :: Read a => GlobalP a -> IO a
readGlobalP (GlobalP f) = exclusiveIO (f ++ ".LOCK") $
  openFile f ReadMode >>= hGetContents >>= return . read

--- Safely reads the current value of a global.
--- If `readGlobalP` fails (e.g., due to a corrupted persistent storage),
--- the global is re-initialized with the default value given as
--- the second argument.
safeReadGlobalP :: (Read a, Show a) => GlobalP a -> a -> IO a
safeReadGlobalP g dflt =
  catch (readGlobalP g) (\_ -> writeGlobalP g dflt >> return dflt)

--- Updates the value of a persistent global entity.
writeGlobalP :: Show a => GlobalP a -> a -> IO ()
writeGlobalP (GlobalP f) v =
  exclusiveIO (f ++ ".LOCK") $ writeFile f (show v ++ "\n")

------------------------------------------------------------------------
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

------------------------------------------------------------------------
