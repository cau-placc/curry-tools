------------------------------------------------------------------------------
--- This library contains the implementation of a pretty-printer for
--- JSON values so that one can show these values in the standard textual
--- format.
---
--- @author Jonas Oberschweiber, Michael Hanus
--- @version January 2025
------------------------------------------------------------------------------

module JSON.Pretty (ppJSON, ppJValue) where

import Data.Char ( intToDigit )

import JSON.Data
import Text.Pretty

--- Pretty print a JSON value with the default options of Curry's Pretty module.
ppJSON :: JValue -> String
ppJSON j = pPrint (ppJValue j)

--- Turn a JSON value into a Doc from Curry's Pretty module.
--- JSON numbers are printed as integers if appropriate.
ppJValue :: JValue -> Doc
ppJValue JTrue        = text "true"
ppJValue JFalse       = text "false"
ppJValue JNull        = text "null"
ppJValue (JNumber x)  = let i = round x
                        in if fromInt i == x then int i else float x
ppJValue (JString s)  = text $ showJSONString s
ppJValue (JArray vs)  = ppJArray vs
ppJValue (JObject ps) = ppJObject ps

ppJArray :: [JValue] -> Doc
ppJArray vs = listSpaced $ map ppJValue vs

ppJObject :: [(String, JValue)] -> Doc
ppJObject ps =
  (nest 2 $ lbrace $$ vsep (punctuate comma $ map ppKVP ps)) $$ rbrace
 where ppKVP (k, v) = (text $ show k) <> colon <+> ppJValue v

-- Show a JSON string with its specific escaping rules.
showJSONString :: String -> String
showJSONString s = '"' : concatMap showJChar s ++ "\""
 where
  showJChar c | c == '"'  = "\\\""
              | c == '\\' = "\\\\"
              | c == '\b' = "\\b"
              | c == '\f' = "\\f"
              | c == '\n' = "\\n"
              | c == '\r' = "\\r"
              | c == '\t' = "\\t"
              | ord c < 32 || ord c > 126 = "\\u" ++ showHex4 (ord c)
              | otherwise                 = [c]

  showHex4 n = map (\d -> intToDigit ((n `div` d) `mod` 16)) [4096, 256, 16, 1]
