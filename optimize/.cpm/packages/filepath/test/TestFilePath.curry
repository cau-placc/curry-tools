-----------------------------------------------------------------------------
-- A few tests for module System.FilePath
-----------------------------------------------------------------------------

import Test.Prop

import Data.List

import System.FilePath

sp1 :: [String]
sp1 = ["Dir1","Dir2","Dir3"]

testSplitSearchPath :: Prop
testSplitSearchPath =
  splitSearchPath (intercalate [searchPathSeparator] sp1) -=- sp1

splitExtProp s = uncurry (++) (splitExtension s) -=- s

addSplitProp s = uncurry addExtension (splitExtension s) -=- s

testSplitExt1 = splitExtension "file.txt" -=- ("file",".txt")
testSplitExt2 = splitExtension "file" -=- ("file","")
testSplitExt3 = splitExtension "file/file.txt" -=- ("file/file",".txt")
testSplitExt4 = splitExtension "file.txt/boris" -=- ("file.txt/boris","")
testSplitExt5 =
  splitExtension "file.txt/boris.ext" -=- ("file.txt/boris",".ext")
testSplitExt6 =
  splitExtension "file/path.txt.bob.fred" -=- ("file/path.txt.bob",".fred")
testSplitExt7 = splitExtension "file/path.txt/" -=- ("file/path.txt/","")


testReplExt1 = replaceExtension "file.txt" ".bob" -=- "file.bob"
testReplExt2 = replaceExtension "file.txt" "bob" -=- "file.bob"
testReplExt3 = replaceExtension "file" ".bob" -=- "file.bob"
testReplExt4 = replaceExtension "file.txt" "" -=- "file"
testReplExt5 = replaceExtension "file.fred.bob" "txt" -=- "file.fred.txt"
