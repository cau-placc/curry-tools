# JSON

This package provides data types, a parser and a pretty printer for [JSON][1].

## Representing JSON values in Curry

A JSON value can be a primitive, i.e. `true`, `false`, `null`, a string or a 
number, an array of JSON values or an object mapping strings to JSON values. In
Curry, a JSON value is represented by the data type `JValue` from the 
`JSON.Data` module:

```haskell
data JValue = JTrue
            | JFalse
            | JNull
            | JString String
            | JNumber Float
            | JArray [JValue]
            | JObject [(String, JValue)]
```

## Parsing JSON strings

`parseJSON` from `JSON.Parser` can be used to parse a JSON string into a 
`JValue`:

```haskell
> parseJSON "{ \"hello\": [\"world\", \"kiel\"] }"
Just (JObject [("hello", JArray [JString "world", JString "kiel"])])
```

## Printing JSON strings

`ppJSON` from `JSON.Pretty` will turn a `JValue` into a pretty printed string.
If you want more control over the layout of the resulting string, you can use
`ppJValue` from the same value to obtain a `Doc` for Curry's `Pretty` module 
from a `JValue`. 

[1]: http://www.json.org
