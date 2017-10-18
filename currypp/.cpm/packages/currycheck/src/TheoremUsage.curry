------------------------------------------------------------------------
--- This module contains some operations to access and check proof
--- for theorems formulated as properties in Curry programs.
---
--- Current assumptions:
--- * A theorem is represented in a source file as a EasyCheck property, e.g.:
---
---       sortPreservesLength xs = length xs -=- length (sort xs)
---
--- * A theorem is considered as proven and, thus, not be checked
---   by CurryCheck or the contract wrapper (see `currypp`), if there exists
---   a file named with prefix "Proof" and the name of the theorem, e.g.,
---   `Proof-sortPreservesLength.agda`. The name is not case sensitive,
---   the file name extension is arbitrary and the special characters in the
---   name are ignored. Hence, a proof for `sortlength` could be also stored in
---   a file named `PROOF_sort-perserves-length.smt`.
---
---  * A proof that some operation `f` is deterministic has the name
---    `fIsDeterministic` so that a proof for `last` can be stored in
---    `proof-last-is-deterministic.agda` (and also in some other files).
---
--- @author Michael Hanus
--- @version October 2016
------------------------------------------------------------------------

module TheoremUsage
  ( determinismTheoremFor
  , getProofFiles, existsProofFor, isProofFileName, isProofFileNameFor
  , getTheoremFunctions
  )  where

import AbstractCurry.Types
import AbstractCurry.Select
import Char
import Directory
import Distribution         (lookupModuleSourceInLoadPath, modNameToPath)
import FilePath             (dropExtension, takeDirectory)
import List

import PropertyUsage

------------------------------------------------------------------------
--- The name of a proof of a determinism annotation for the operation
--- given as the argument.
determinismTheoremFor :: String -> String
determinismTheoremFor funcname = funcname ++ "isdeterministic"

------------------------------------------------------------------------
-- Operations for proof files:

--- Get the names of all files in the directory (first argument) containing
--- proofs of theorems.
getProofFiles :: String -> IO [String]
getProofFiles dir = do
  files <- getDirectoryContents dir
  return (filter isProofFileName files)

--- Does the list of file names (second argument) contain a proof of the
--- theorem given as the first argument?
existsProofFor :: String -> [String] -> Bool
existsProofFor propname filenames =
  any (isProofFileNameFor propname) filenames

--- Is this a file name with a proof, i.e., start it with `proof`?
isProofFileName :: String -> Bool
isProofFileName fn = "proof" `isPrefixOf` (map toLower fn)

--- Is this the file name of a proof of property `prop`?
isProofFileNameFor :: String -> String -> Bool
isProofFileNameFor prop fname =
  let lfname = map toLower (dropExtension fname)
      lprop  = map toLower prop
   in if "proof" `isPrefixOf` lfname
      then deleteNonAlphanNum (drop 5 lfname) == deleteNonAlphanNum lprop
      else False

--- Delete non alphanumeric characters.
deleteNonAlphanNum :: String -> String
deleteNonAlphanNum = filter isAlphaNum

------------------------------------------------------------------------
--- Get all theorems which are contained in a given program.
--- A theorem is a property for which a proof file exists in the
--- directory provided as first argument.
getTheoremFunctions :: String -> CurryProg -> IO [CFuncDecl]
getTheoremFunctions proofdir (CurryProg _ _ _ _ _ _ functions _) = do
  let propfuncs = filter isProperty functions -- all properties
  prooffiles <- getProofFiles proofdir
  return $ filter (\fd -> existsProofFor (snd (funcName fd)) prooffiles)
                  propfuncs

------------------------------------------------------------------------
