------------------------------------------------------------------------------
--- Warnings for Markup Language Parsing.
---
--- @author Max Deppert
--- @version March 2014
------------------------------------------------------------------------------
module MLWarning where

import ParseTypes (fromSimplePos,Warning)
import MLTypes (TPos,WarnID(..))

-- push a new warning to an existent list of warnings
warn :: TPos -> WarnID -> [Warning] -> [Warning]
warn pos id ws = (fromSimplePos (fst pos),msg) : ws
  where msg = case id of
         TagNameFirstDigit
          -> "ignored first tag name character because it is a digit"
         TagNameNotAlphaNum
          -> "ignored tag name character because it is not alphanumerical"
         TagEndsUnexpected
          -> "unexpected characters at end of tag"
         UnquotedAttributeEmpty
          -> "unquoted attribute values must not be empty"
         Unquoted c
          -> "ignored " ++ [c] ++ " in unquoted attribute value"
         AttributesUnseperated
          -> "attributes must be seperated by at least one space character"
         UnexpectedEndTag
          -> "ignored unexpected end tag"
         SingleEndTag
          -> "end tag without previous start tag"

-- map a list with a warning function, to get all warnings
wmap :: (a -> (b,[Warning])) -> [a] -> ([b],[Warning])
wmap f ys = wmapper ys
  where wmapper [] = ([],[])
        wmapper (x:xs) =
          let (p,q) = f x
              (a,b) = wmapper xs
           in (p : a,q ++ b)


