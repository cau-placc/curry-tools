------------------------------------------------------------------------------
--- Testing to store handles im temporary globals.
---
--- @author Michael Hanus
------------------------------------------------------------------------------

import Control.Monad  ( when )
import Data.Global
import System.Directory
import System.IO
import Test.Prop

------------------------------------------------------------------------------
-- A temporary global entity holding a file handle:
myHandle :: GlobalT (Maybe Handle)
myHandle = globalT "TestGlobalHandles.myHandle" Nothing

tmpFile :: String
tmpFile = "XXXOUT"

setOutHandle :: IO ()
setOutHandle = do
  h <- openFile tmpFile WriteMode
  writeGlobalT myHandle (Just h)

writeMyFile :: IO ()
writeMyFile =
  readGlobalT myHandle >>= maybe (return ()) (\h -> hPutStrLn h "ABC")

closeMyFile :: IO ()
closeMyFile =
  readGlobalT myHandle >>= maybe (return ()) hClose

setInHandle :: IO ()
setInHandle = do
  h <- openFile tmpFile ReadMode
  writeGlobalT myHandle (Just h)

readMyFile :: IO String
readMyFile =
  readGlobalT myHandle >>= maybe (return "") hGetContents

testGlobalHandles :: PropIO
testGlobalHandles =
 (setOutHandle >> writeMyFile >> closeMyFile >> setInHandle >> readMyFile)
   `returns` "ABC\n"

-- finalize: clean
testCleanUp :: PropIO
testCleanUp =
  (doesFileExist tmpFile >>= \exf -> when exf (removeFile tmpFile)) `returns` ()

------------------------------------------------------------------------------
