module ParserTests exposing (..)

import Edn exposing (..)
import Edn.Parser exposing (edn)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Json.Encode
import Parser exposing (run)
import Test exposing (Test, describe, fuzz, test)


checkParsing : List ( String, Edn ) -> Expectation
checkParsing =
    List.map (Tuple.mapFirst (run edn))
        >> List.map (\( actual, expected ) -> always (Expect.equal actual (Ok expected)))
        >> (\predicates -> Expect.all predicates ())


suite : Test
suite =
    describe "Parsing"
        [ test "booleans" <|
            \_ ->
                checkParsing
                    [ ( "true", EdnBool True )
                    , ( "false", EdnBool False )
                    ]
        , fuzz int "integers" <|
            \val ->
                checkParsing [ ( String.fromInt val, EdnInt val ) ]
        , fuzz string "strings" <|
            \val ->
                checkParsing
                    [ ( Json.Encode.encode 2 (Json.Encode.string val)
                      , EdnString val
                      )
                    ]
        ]
