module JSON.Pretty (ppJSON, ppJValue) where

import JSON.Data
import Text.Pretty

--- Pretty print a JSON value with the default options of Curry's Pretty module.
ppJSON :: JValue -> String
ppJSON j = pPrint (ppJValue j)

--- Turn a JSON value into a Doc from Curry's Pretty module. 
ppJValue :: JValue -> Doc
ppJValue JTrue = text "true"
ppJValue JFalse = text "false"
ppJValue JNull = text "null"
ppJValue (JNumber f) = float f
ppJValue (JString s) = text $ show s
ppJValue (JArray vs) = ppJArray vs
ppJValue (JObject ps) = ppJObject ps

ppJArray :: [JValue] -> Doc
ppJArray vs = listSpaced $ map ppJValue vs

ppJObject :: [(String, JValue)] -> Doc
ppJObject ps = (nest 2 $ lbrace $$ vsep (punctuate comma $ map ppKVP ps)) $$ rbrace
  where ppKVP (k, v) = (text $ show k) <> colon <+> ppJValue v
