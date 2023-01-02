# Elm Edn Parser

A library for parsing
[extensible data notation, "Edn"](https://github.com/edn-format/edn) in Elm!

This is very useful for communicating between Clojure and Elm.

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
```

# Decoding/Encoding

This library doesn't provide Encoders and Decoders for the types in the `Edn` module.

Extensible Data Notation has a few interesting types that don't necessarily translate 1 to 1
without making some decisions about how that transform happens.

* Edn maps can have keys that are not strings. The `EdnMap` type in this library represents
them as `List (Edn, Edn)`.
* Edn has sets that can contain more than what Elm considers "comparable". The `EdnSet` type in this
library represents them as `List Edn`.
* Tags are a cool feature of Edn that can instruct a client consuming the data how to handle that
tagged data differently. There are a couple different possible ways a decoder library can 
represent this.

So there's several things about .edn that manifest as implementation details for decoder libraries
to deal with. I plan to create a decoder/encode package to accompany this, but want to keep it
separate in case anyone wants to write their own that handles things differently than I do.

# Prior Art

The tests in this library and the escaped unicode parsing are largely drawn from
[Robert Vollmert's](https://github.com/robx) excellent 
[elm-edn](https://github.com/robx/elm-edn) library.