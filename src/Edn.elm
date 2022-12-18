module Edn exposing (..)

import Array exposing (Array)


type Edn
    = EdnString String
    | EdnVariable String
    | EdnKeyword ( Maybe String, String )
    | EdnList (List Edn)
    | EdnVector (Array Edn)
    | EdnMap (List ( Edn, Edn ))
    | EdnSet (List Edn)
    | EdnNil
    | EdnBool Bool
    | EdnInt Int
    | EdnFloat Float
    | EdnChar Char
    | EdnTag ( String, String ) Edn
