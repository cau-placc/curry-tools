------------------------------------------------------------------------------
--- This library defines a type class and instances for standard types
--- to convert Curry values to JSON values and vice versa.
---
--- @author Michael Hanus
--- @version October 2024
------------------------------------------------------------------------------

module JSON.Convert where

import Data.Maybe ( catMaybes, isJust )
import JSON.Data

--- Type class with two conversion operations between values and their
--- JSON representation. Since a JSON value might not contain
--- a correct representation of a standard value, the operation
--- `fromJSON` returns a `Maybe` value.
--- The additional operations on value lists are used for a better
--- JSON conversion of strings.
class ConvertJSON a where
  toJSON :: a -> JValue
  fromJSON :: JValue -> Maybe a

  toJSONList :: [a] -> JValue
  fromJSONList :: JValue -> Maybe [a]

  toJSONList = JArray . map toJSON

  fromJSONList jv = case jv of
    JArray xs -> let ys = map fromJSON xs
                 in if all isJust ys then Just (catMaybes ys)
                                     else Nothing
    _         -> Nothing

--- Instance for Booleans.
instance ConvertJSON Bool where
  toJSON False = JFalse
  toJSON True  = JTrue

  fromJSON jv = case jv of
    JFalse -> Just False
    JTrue  -> Just True
    _      -> Nothing

--- Instance for characters and strings.
instance ConvertJSON Char where
  toJSON c = JString [c]

  fromJSON jv = case jv of
    JString [c] -> Just c
    _           -> Nothing

  toJSONList s = JString s

  fromJSONList jv = case jv of
    JString s -> Just s
    _         -> Nothing

--- Instance for floats.
instance ConvertJSON Float where
  toJSON x = JNumber x

  fromJSON jv = case jv of
    JNumber n -> Just n
    _         -> Nothing

--- Instance for integers.
--- Use rounding to check whether it is a reasonable integer.
instance ConvertJSON Int where
  toJSON x = JNumber (fromInt x)

  fromJSON jv = case jv of
    JNumber x -> let i = round x
                 in if fromInt i == x then Just i else Nothing
    _         -> Nothing

--- Instance for lists.
instance ConvertJSON a => ConvertJSON [a] where
  toJSON = toJSONList

  fromJSON = fromJSONList

--- Instance for `Maybe` values.
instance ConvertJSON a => ConvertJSON (Maybe a) where
  toJSON Nothing  = JNull
  toJSON (Just x) = toJSON x

  fromJSON jv = case jv of
    JNull -> Just Nothing
    _     -> fmap Just (fromJSON jv)

--- Instance for `Either` values.
instance (ConvertJSON a, ConvertJSON b) => ConvertJSON (Either a b) where
  toJSON (Left  x) = JObject [("Left",  toJSON x)]
  toJSON (Right y) = JObject [("Right", toJSON y)]

  fromJSON jv = case jv of
    JObject [("Left", v)] -> fmap Left  (fromJSON v)
    JObject [("Right",v)] -> fmap Right (fromJSON v)
    _                     -> Nothing

--- Instance for `Ordering` values.
instance ConvertJSON Ordering where
  toJSON x = JString (show x)

  fromJSON jv = case jv of
    JString s -> case reads s of [(x,"")] -> Just x
                                 _        -> Nothing
    _         -> Nothing

--- Instance for pairs of values.
instance (ConvertJSON a, ConvertJSON b) => ConvertJSON (a,b) where
  toJSON (x,y) = JObject [("1", toJSON x), ("2", toJSON y)]

  fromJSON jv = case jv of
    JObject [("1",v1), ("2",v2)] -> do x <- fromJSON v1
                                       y <- fromJSON v2
                                       return (x,y)
    _                            -> Nothing
