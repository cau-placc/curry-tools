-- Some simple tests for the JSON package.

import Data.Maybe
import Test.Prop

import JSON.Data
import JSON.Parser
import JSON.Pretty

-- A simple JSON value.
jsonDoc :: JValue
jsonDoc = JObject [("hello", JArray [JString "world", JString "kiel"])]

-- Parsing JSON strings.
testParse :: Prop
testParse = parseJSON "{ \"hello\": [\"world\", \"kiel\"] }" -=- Just jsonDoc

-- Test pretty printing and parsing.
testPrint :: Prop
testPrint = parseJSON (ppJSON jsonDoc) -=- Just jsonDoc

-- Equality of JSON values where float equality is approximated.
eqJValue :: JValue -> JValue -> Bool
eqJValue jv1 jv2 = case (jv1,jv2) of
  (JNumber x, JNumber y) -> abs (x - y) < 0.0001
  (JArray xs, JArray ys) -> all (uncurry eqJValue) (zip xs ys)
  (JObject xs,JObject ys) -> all eqComp (zip xs ys)
  _                       -> jv1 == jv2
 where
  eqComp ((s,x),(t,y)) = s == t && eqJValue x y

-- Test property of pretty printing and parsing.
testParsePrint :: JValue -> Prop
testParsePrint json =
  always (eqJValue (fromJust (parseJSON (ppJSON json))) json)
