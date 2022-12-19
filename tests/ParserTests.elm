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


stringWithNewLine =
    """"a
  b
\""""


suite : Test
suite =
    describe "Parsing"
        [ test "booleans" <|
            \_ ->
                checkParsing
                    [ ( "true", EdnBool True )
                    , ( "false", EdnBool False )
                    ]
        , fuzz int "fuzz integers" <|
            \val ->
                checkParsing [ ( String.fromInt val, EdnInt val ) ]
        , test "strings" <|
            \_ ->
                checkParsing
                    [ ( Json.Encode.encode 0 (Json.Encode.string ""), EdnString "" )
                    , ( Json.Encode.encode 0 (Json.Encode.string "\\"), EdnString "\\" )
                    , ( Json.Encode.encode 0 (Json.Encode.string "\n"), EdnString "\n" )
                    , ( "\"a string\\twith\\\\escape\\\"'s\"", EdnString "a string\twith\\escape\"'s" )
                    , ( """ "me & you \\u0026 them" """, EdnString "me & you & them" )
                    , ( stringWithNewLine, EdnString "a\n  b\n" )
                    ]
        ]
