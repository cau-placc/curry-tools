------------------------------------------------------------------------------
--- This library contains the implementation of a parser for JSON values,
--- i.e., an operation `parseJSON` which reads a textual JSON representation
--- and returns a `Maybe` value of type `JValue`.
---
--- @author Jonas Oberschweiber
--- @version September 2024
------------------------------------------------------------------------------

module JSON.Parser (parseJSON) where

import JSON.Data
import Data.Char
import DetParse
import Test.Prop

import Prelude hiding (some, empty, (<|>), (<$>), (<*>), (<*), (*>))

--- Parses a JSON string into a JValue. Returns Nothing if the string could not
--- be parsed.
parseJSON :: String -> Maybe JValue
parseJSON = parse pJValue

--- Parser for a JValue
pJValue :: Parser JValue
pJValue =   pTrue
        <|> pFalse
        <|> pNull
        <|> pJString
        <|> pJNumber
        <|> pArray
        <|> pObject

pObject :: Parser JValue
pObject =
      JObject <$>
        (char '{' *> pWhitespace *> pObject' <* pWhitespace <* char '}'
           <* pWhitespace)
  <|> JObject <$> (char '{' *> pWhitespace *> char '}' *> yield [])

pObject' :: Parser [(String, JValue)]
pObject' =
  (:) <$> (pWhitespace *> pKeyValuePair) <*>
  (pWhitespace *> char ',' *> pObject' <|> yield [])

pKeyValuePair :: Parser (String, JValue)
pKeyValuePair =
  (,) <$> pString <*> (pWhitespace *> char ':' *> pWhitespace *> pJValue)

test_pObject_empty :: Prop
test_pObject_empty = parse pObject "{}" -=- Just (JObject [])

test_pObject_onlyStringKeys :: Prop
test_pObject_onlyStringKeys = parse pObject "{1: 2}" -=- Nothing

test_pObject_simple :: Prop
test_pObject_simple = parse pObject "{\"test\": 1, \"test2\": false}" -=- Just (JObject [("test", JNumber 1.0), ("test2", JFalse)])

test_pObject_whitespace :: Prop
test_pObject_whitespace = parse pObject "{\n \"test\": 1,\n \"test2\": false\n}" -=- Just (JObject [("test", JNumber 1.0), ("test2", JFalse)])

test_pObject_nested :: Prop
test_pObject_nested = parse pObject "{\"test\": {\"hello\": \"world\"}}" -=- Just (JObject [("test", JObject [("hello", JString "world")])])

pArray :: Parser JValue
pArray =   JArray <$> (char '[' *> pWhitespace *> pArray' <* pWhitespace <* char ']')
       <|> JArray <$> (char '[' *> pWhitespace *> char ']' *> yield [])

pArray' :: Parser [JValue]
pArray' = (:) <$> (pWhitespace *> pJValue) <*> ((pWhitespace *> char ',' *> pArray') <|> yield [])

test_pArray_empty :: Prop
test_pArray_empty = parse pArray "[]" -=- Just (JArray [])

test_pArray_single :: Prop
test_pArray_single = parse pArray "[1]" -=- Just (JArray [JNumber 1.0])

test_pArray_multi :: Prop
test_pArray_multi = parse pArray "[true, false, null]" -=- Just (JArray [JTrue, JFalse, JNull])

test_pArray_nested :: Prop
test_pArray_nested = parse pArray "[true, [false], [[null]]]" -=- Just (JArray [JTrue, JArray [JFalse], JArray [JArray [JNull]]])

pWhitespace :: Parser ()
pWhitespace =   char ' ' *> pWhitespace
            <|> char '\n' *> pWhitespace
            <|> char '\r' *> pWhitespace
            <|> char '\t' *> pWhitespace
            <|> empty

pTrue :: Parser JValue
pTrue = word "true" *> yield JTrue

pFalse :: Parser JValue
pFalse = word "false" *> yield JFalse

pNull :: Parser JValue
pNull = word "null" *> yield JNull

pJString :: Parser JValue
pJString = JString <$> pString

pString :: Parser String
pString = char '"' *> pCharSequence <* char '"'

pCharSequence :: Parser String
pCharSequence =
      (++) <$> (char '\\' *> (pEscaped <!> failure)) <*> pCharSequence
  <!> (:) <$> check (\c -> c /= '"' && c /= '\\') anyChar <*> pCharSequence
  <!> yield ""

pEscaped :: Parser String
pEscaped =   char '"'  *> yield "\""
         <|> char '\\' *> yield "\\"
         <|> char '/'  *> yield "/"
         <|> char 'b'  *> yield "\b"
         <|> char 'f'  *> yield "\f"
         <|> char 'n'  *> yield "\n"
         <|> char 'r'  *> yield "\r"
         <|> char 't'  *> yield "\t"
         <|> ((:[]) . chr) <$> (char 'u' *> pTwoByteHex)

pTwoByteHex :: Parser Int
pTwoByteHex =
  hexToInt <$>
  ((:) <$> pHexDigit <*> ((:) <$> pHexDigit <*> ((:)
       <$> pHexDigit <*> ((:[]) <$> pHexDigit))))
 where pHexDigit = check isHexDigit anyChar

hexToInt :: String -> Int
hexToInt s = foldl1 ((+) . (16*)) (map digitToInt s)

test_pCharSequence_simple :: Prop
test_pCharSequence_simple = parse pCharSequence "test" -=- Just "test"

test_pCharSequence_noDoubleQuote :: Prop
test_pCharSequence_noDoubleQuote = parse pCharSequence "te\"st" -=- Nothing

test_pCharSequence_noStandaloneBackslash :: Prop
test_pCharSequence_noStandaloneBackslash = parse pCharSequence "He\\world" -=- Nothing

test_pCharSequence_escapedDoubleQuote :: Prop
test_pCharSequence_escapedDoubleQuote = parse pCharSequence "Hello \\\"World\\\"" -=- Just "Hello \"World\""

test_pCharSequence_escapedBackslash :: Prop
test_pCharSequence_escapedBackslash = parse pCharSequence "He\\\\world" -=- Just "He\\world"

test_pCharSequence_escapedSlash :: Prop
test_pCharSequence_escapedSlash = parse pCharSequence "He\\/world" -=- Just "He/world"

test_pCharSequence_escapedBackspace :: Prop
test_pCharSequence_escapedBackspace = parse pCharSequence "He\\bworld" -=- Just "He\bworld"

test_pCharSequence_escapedFormFeed :: Prop
test_pCharSequence_escapedFormFeed = parse pCharSequence "He\\fworld" -=- Just "He\fworld"

test_pCharSequence_escapedNewline :: Prop
test_pCharSequence_escapedNewline = parse pCharSequence "He\\nworld" -=- Just "He\nworld"

test_pCharSequence_escapedCarriageReturn :: Prop
test_pCharSequence_escapedCarriageReturn = parse pCharSequence "He\\rworld" -=- Just "He\rworld"

test_pCharSequence_escapedTab :: Prop
test_pCharSequence_escapedTab = parse pCharSequence "He\\tworld" -=- Just "He\tworld"

test_pCharSequence_twoEscapes :: Prop
test_pCharSequence_twoEscapes = parse pCharSequence "He\\r\\nWorld" -=- Just "He\r\nWorld"

test_pCharSequence_escapedUnicodeChar :: Prop
test_pCharSequence_escapedUnicodeChar = parse pCharSequence "Hello \\u2603 World" -=- Just "Hello ☃ World"

test_pCharSequence_escapedUnicodeRequiresFourDigits :: Prop
test_pCharSequence_escapedUnicodeRequiresFourDigits = parse pCharSequence "Hello \\u26 World" -=- Nothing

test_pString_simple :: Prop
test_pString_simple = parse pString "\"Hello, World\"" -=- Just "Hello, World"

test_pString_complex :: Prop
test_pString_complex = parse pString "\"Hello \\r\\n \\u2603 World\"" -=- Just "Hello \r\n ☃ World"

pJNumber :: Parser JValue
pJNumber = JNumber <$> pNumber

pNumber :: Parser Float
pNumber =   (0-) <$> (char '-' *> pPositiveFloat)
        <|> pPositiveFloat

-- number without decimal point, decimal digits, base 10 exponent
toFloat :: Int -> Int -> Int -> Float
toFloat n d e = (fromInt n) * (exp 10 (d + e))
 where exp x y = if y < 0 then 1 / (x ^ (0 - y)) else (x ^ y)

pPositiveFloat :: Parser Float
pPositiveFloat = (uncurry toFloat) <$> pWithDecimalPoint <*> pExponent

pExponent :: Parser Int
pExponent =
      (char 'e' <|> char 'E') *>
       (char '-' *> yield negate <|> char '+' *> yield id <|> yield id) <*> pInt
  <!> yield 0

pWithDecimalPoint :: Parser (Int, Int)
pWithDecimalPoint =
  combine <$> some pDigit <*> (char '.' *> some pDigit <|> yield "")
 where
  s2i cs = foldl1 ((+).(10*)) (map (\c' -> ord c' - ord '0') cs)
  combine n d = (s2i (n ++ d), negate $ length d)

pInt :: Parser Int
pInt =
  (\cs -> foldl1 ((+).(10*)) (map (\c' -> ord c' - ord '0') cs)) <$> some pDigit

pDigit :: Parser Char
pDigit = check isDigit anyChar
