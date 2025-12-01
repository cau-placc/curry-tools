------------------------------------------------------------------------------
--- Some tests for library `FlatCurry.XML`.
---
--- @author Michael Hanus
------------------------------------------------------------------------------

import FlatCurry.Types
import FlatCurry.Files
import FlatCurry.XML
import XML
import Test.Prop

-- Shows a program in XML format:
showxml mod = do
  prog <- readFlatCurry mod
  putStrLn $ showXmlDoc (flatCurry2Xml prog)

-- Store a program in XML format:
store mod = do
  prog <- readFlatCurry mod
  flatCurry2XmlFile (mod ++ "_fcy.xml") prog
  putStrLn (mod ++ "_fcy.xml" ++ " written")

-- Test for equality after XML encoding/decoding:
testEqualFcy prog = prog == xml2FlatCurry (flatCurry2Xml prog)

readAndTestEqualFcy mod = do
  prog <- readFlatCurry mod
  return $ testEqualFcy prog


testXML_test_for_rev = (readAndTestEqualFcy "rev") `returns` True
