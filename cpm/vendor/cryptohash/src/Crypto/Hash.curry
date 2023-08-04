----------------------------------------------------------------------------
--- This library contains operations to support simple
--- cryptography hashing. Currently, it is based on Unix tools.
---
--- @author Michael Hanus
----------------------------------------------------------------------------

module Crypto.Hash
  ( getHash, getHashWith, randomString )
 where

import System.IO     ( hClose, hGetLine, hPutStrLn )
import System.IOExts ( execCmd )

import System.Random ( getRandomSeed, nextInt )

--------------------------------------------------------------------------
-- Operations for hashing.

--- Default hashing function.
--- @param toHash - string which should be hashed
--- @return the hashSum of this str
getHash :: String -> IO String
getHash = getHashWith "md5sum"
--getHash = getHashWith "sha1sum"

--- Hashes a string with an explicit Unix hash command.
--- @param hashcmd - Unix command for hasing
--- @param toHash - string which should be hashed
--- @return the hashed string
getHashWith :: String -> String -> IO String
getHashWith hashcmd toHash = do
  (sin, sout, _) <- execCmd hashcmd
  hPutStrLn sin toHash
  hClose sin
  result <- hGetLine sout
  return (head (words result))

--- Returns a random string (a hexadecimal string) of a particular length.
--- @param length - length of the desired string
--- @return the random string
randomString :: Int -> IO String
randomString n = do
  seed <- getRandomSeed
  ranString <- getHash (show (nextInt seed !! 3))
  return (take n ranString)

--------------------------------------------------------------------------
