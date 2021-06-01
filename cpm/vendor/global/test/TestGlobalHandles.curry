------------------------------------------------------------------------------
--- Testing to store handles im temporary globals.
---
--- @author Michael Hanus
------------------------------------------------------------------------------

import Data.Global
import System.IO
import System.Process ( system )
import Test.Prop

------------------------------------------------------------------------------
-- A temporary global entity holding a file handle:
myHandle :: GlobalT (Maybe Handle)
myHandle = globalT "TestGlobalHandles.myHandle" Nothing

setOutHandle :: IO ()
setOutHandle = do
  h <- openFile "XXXOUT" WriteMode
  writeGlobalT myHandle (Just h)

writeMyFile :: IO ()
writeMyFile =
  readGlobalT myHandle >>= maybe (return ()) (\h -> hPutStrLn h "ABC")

closeMyFile :: IO ()
closeMyFile =
  readGlobalT myHandle >>= maybe (return ()) hClose

setInHandle :: IO ()
setInHandle = do
  h <- openFile "XXXOUT" ReadMode
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
testCleanUp = (system "rm -f XXXOUT") `returns` 0

------------------------------------------------------------------------------
