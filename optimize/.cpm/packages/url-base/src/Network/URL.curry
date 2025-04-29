------------------------------------------------------------------------------
--- Base library for dealing with URLs (Uniform Resource Locators).
--- 
--- Currently, it contains operations for encoding and decoding strings
--- to and from URL encoded format which are useful to construct URLs
--- with parameter strings.
---
--- @author Michael Hanus
--- @version April 2025
------------------------------------------------------------------------------

module Network.URL
  ( string2urlencoded, param2urlencoded, params2urlencoded
  , urlencoded2string )
 where

import Data.Char ( isAlphaNum )
import Data.List ( intercalate )
import Numeric   ( readHex )
import Test.Prop

------------------------------------------------------------------------------
-- Encoding:

--- Translates strings into equivalent URL encoded strings.
--- See also <http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1>.
string2urlencoded :: String -> String
string2urlencoded [] = []
string2urlencoded (c:cs)
  | noEncode c = c : string2urlencoded cs
  | c == ' '   = '+' : string2urlencoded cs
  | otherwise
  = let oc = ord c
    in '%' : int2hex(oc `div` 16) : int2hex(oc `mod` 16) : string2urlencoded cs
 where
  noEncode x = isAlphaNum x || x `elem` "-"

  int2hex i = if i<10 then chr (ord '0' + i)
                      else chr (ord 'A' + i - 10)

--- Translates a parameter (name/value pair) into an URL encoded string
--- where both components are URL encoded and separated by `=`.
--- The separator is omitted if the value is the empty string.
--- See also <http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1>.
param2urlencoded :: (String,String) -> String
param2urlencoded (n,v)
  | null v    = string2urlencoded n
  | otherwise = string2urlencoded n ++ '=' : string2urlencoded v

--- Translates a list of parameters (name/value pairs)
--- into URL encoded strings where the parameters are separated by `&`.
--- See also <http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1>.
params2urlencoded :: [(String,String)] -> String
params2urlencoded ps = intercalate "&" (map param2urlencoded ps)

------------------------------------------------------------------------------
-- Decoding:

--- Translates an URL encoded string into an equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string []     = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (case readHex (take 2 cs) of [(n,"")] -> n
                                                 _        -> 0)
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

------------------------------------------------------------------------------

-- Test whether encoding and decoding yields identical results.
testUrlEnconding :: String -> Prop
testUrlEnconding s = urlencoded2string (string2urlencoded s) -=- s

------------------------------------------------------------------------------
