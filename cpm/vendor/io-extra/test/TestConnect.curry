-- Testing operations from library IOExts:

import System.IO
import System.IOExts
import Test.Prop

-- Execute shell command show the first output line of its execution:
getExec :: String -> IO String
getExec cmd = do
  hdl <- connectToCommand cmd
  s <- hGetLine hdl
  hClose hdl
  return s

testConnectToCommand :: PropIO
testConnectToCommand = (getExec "echo abcde") `returns` "abcde"
