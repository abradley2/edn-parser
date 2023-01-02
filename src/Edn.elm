module Edn exposing (Edn(..))

{-| The main union type representing an Extensible Data Notation document.


# Definition

@docs Edn

-}

import Array exposing (Array)


{-| Union type representing different EDN values. There are a few noteworthy cases
where data structures differ slightly from their EDN counterparts. There are no
equivalent "Maps" in Elm, so `EdnMap` is represented as `(List (Edn, Edn))`.

Similarly, a set is just a `List Edn` type because sets in Elm must be made from
comparables.

Keywords are a tuple with the first item being an optional namespace.

Tags start with a tuple that contain the _required_ namespace and tag name consecutively,
before the EDN value that they annotate.

-}
type Edn
    = EdnString String
    | EdnVariable String
    | EdnKeyword (Maybe String) String
    | EdnList (List Edn)
    | EdnVector (Array Edn)
    | EdnMap (List ( Edn, Edn ))
    | EdnSet (List Edn)
    | EdnNil
    | EdnBool Bool
    | EdnInt Int
    | EdnFloat Float
    | EdnChar Char
    | EdnTag String String Edn
