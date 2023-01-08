# Elm Edn Parser

A library for parsing
[extensible data notation, "Edn"](https://github.com/edn-format/edn) in Elm!

This is very useful for communicating between Clojure and Elm.

This is just for _parsing_ edn data into a `Edn` elm type. For full decoding check out
[edn-decode](https://package.elm-lang.org/packages/abradley2/edn-decode/latest/)

# The "Edn" Type

```elm
type Edn
    = EdnString String
    | EdnSymbol String
    -- keywords are represented as strings with optional namespace
    | EdnKeyword (Maybe String) String
    | EdnList (List Edn)
    | EdnVector (Array Edn)
    -- maps in Edn can have keys that are not strings, so we represent them as a list
    -- of tuples here, leaving the translation to a map up to decoders
    | EdnMap (List ( Edn, Edn ))
    -- sets must consist of predefined comparables in Elm, so it's represented
    -- as a list on this type, leaving the translation to a set up to decoders
    | EdnSet (List Edn)
    | EdnNil
    | EdnBool Bool
    | EdnInt Int
    | EdnFloat Float
    | EdnChar Char
    -- user defined tags have a namespace and a name, followed by a nested edn value
    | EdnTag String String Edn
    | EdnSymbol String
```

# Usage

```elm
import Edn exposing (Edn)
import Edn.Parser exposing (Error)

result : Result Error Edn
result = Edn.Parser.run """(1 true {:hello "world"})""" 
```

# Decoding/Encoding

This library doesn't provide Encoders and Decoders for the types in the `Edn` module.

Extensible Data Notation has a few interesting types that don't necessarily translate cleanly to Elm
without making some decisions about how that transform happens.

For full decoding check out
[edn-decode](https://package.elm-lang.org/packages/abradley2/edn-decode/latest/)
or any other library that implements decoders for the `Edn` type.

# Prior Art

The tests in this library and the escaped unicode parsing are largely drawn from
[Robert Vollmert's](https://github.com/robx) excellent 
[elm-edn](https://github.com/robx/elm-edn) library.