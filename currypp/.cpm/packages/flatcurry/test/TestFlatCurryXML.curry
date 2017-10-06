------------------------------------------------------------------------------
--- Some tests for library `FlatCurry.XML`.
---
--- @author Michael Hanus
------------------------------------------------------------------------------

import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.XML
import SearchTree(someValue)
import XML
import Test.Prop

-- Shows a program in XML format:
showxml mod = do
  prog <- readFlatCurry mod
  putStrLn (someValue (showXmlDoc (flatCurry2Xml prog)))

-- Store a program in XML format:
store mod = do
  prog <- readFlatCurry mod
  flatCurry2XmlFile prog (mod++"_fcy.xml")
  putStrLn (mod++"_fcy.xml"++" written")

-- Test for equality after XML encoding/decoding:
testEqualFcy prog = prog == xml2FlatCurry (flatCurry2Xml prog)

readAndTestEqualFcy mod = do
  prog <- readFlatCurry mod
  return (someValue (testEqualFcy prog))


testXML_test_for_rev = (readAndTestEqualFcy "rev") `returns` True
