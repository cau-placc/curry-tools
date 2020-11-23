-- Testing operations from library IOExts:

import System.IO
import System.IOExts
import Test.Prop

-- Execute shell command show the first output line of its execution:
getExec cmd = do
  hdl <- connectToCommand cmd
  s <- hGetLine hdl
  hClose hdl
  return s

testConnectToCommand = (getExec "echo abcde") `returns` "abcde"
