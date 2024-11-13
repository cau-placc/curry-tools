-- Tests for the module JSON.Convert

module TestConvert where

import Test.Prop

import JSON.Data
import JSON.Convert

-----------------------------------------------------------------------------
-- Test cases to check the binary representation against the built-in integer
-- operations.
-----------------------------------------------------------------------------

test_convertInt :: Int -> Prop
test_convertInt x = fromJSON (toJSON x) -=- Just x

test_convertFloat :: Float -> Prop
test_convertFloat x = fromJSON (toJSON x) -=- Just x

test_convertMaybeBool :: Maybe Bool -> Prop
test_convertMaybeBool x = fromJSON (toJSON x) -=- Just x

test_convertList :: [String] -> Prop
test_convertList x = fromJSON (toJSON x) -=- Just x

test_convertPair :: (Char,String) -> Prop
test_convertPair x = fromJSON (toJSON x) -=- Just x
