module ParserTests exposing (..)

import Array
import Dict
import Edn exposing (..)
import Edn.Parser exposing (Definitions, edn)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Json.Encode
import Parser exposing (run)
import Set
import Test exposing (Test, describe, fuzz, test)
import TestCases


expectError : Result error value -> Expectation
expectError r =
    case r of
        Ok _ ->
            Expect.fail "Expected error"

        Err _ ->
            Expect.pass


checkParsing : List ( String, Edn ) -> Expectation
checkParsing =
    List.map (Tuple.mapFirst (run edn))
        >> List.map (\( actual, expected ) -> always (Expect.equal actual (Ok expected)))
        >> (\predicates -> Expect.all predicates ())


stringWithNewLine : String
stringWithNewLine =
    """"a
  b
\""""


ednWithTags : String
ednWithTags =
    """{:sum #test-app/int-add (6 3)}"""


type TestAppError
    = InvalidIntSumArg Edn
    | ParserError Edn.Parser.Error


testAppDefinitions : Definitions TestAppError
testAppDefinitions =
    ( "test-app"
    , Dict.fromList
        [ ( "int-add"
          , \args ->
                case args of
                    EdnList ((EdnInt a) :: (EdnInt b) :: []) ->
                        Ok (EdnInt (a + b))

                    _ ->
                        Err (InvalidIntSumArg args)
          )
        ]
    )


suite : Test
suite =
    describe "Parsing"
        [ test "taggedMap" <|
            \_ ->
                """#my-tag{:foo 1 :bar 2}"""
                    |> Edn.Parser.run
                    |> Expect.equal
                        (Ok <|
                            EdnMap
                                [ ( EdnKeyword (Just "my-tag") "foo", EdnInt 1 )
                                , ( EdnKeyword (Just "my-tag") "bar", EdnInt 2 )
                                ]
                        )
        , test "applyDataReader on tags" <|
            \_ ->
                ednWithTags
                    |> Edn.Parser.run
                    |> Result.mapError ParserError
                    |> Result.andThen (Edn.Parser.applyDataReaders testAppDefinitions)
                    |> Result.map (Expect.equal (EdnMap [ ( EdnKeyword Nothing "sum", EdnInt 9 ) ]))
                    |> Result.withDefault (Expect.fail "Failed to parse")
        , test "Ignore unknown tags" <|
            \_ ->
                """#tag/unknown 1"""
                    |> Edn.Parser.run
                    |> Result.mapError ParserError
                    |> Result.andThen (Edn.Parser.applyDataReaders testAppDefinitions)
                    |> Result.map (Expect.equal (EdnTag "tag" "unknown" (EdnInt 1)))
                    |> Result.withDefault (Expect.fail "Failed to parse")
        , test "booleans" <|
            \_ ->
                checkParsing
                    [ ( "true", EdnBool True )
                    , ( "false", EdnBool False )
                    ]
        , fuzz float "fuzz floats" <|
            \val ->
                case String.toInt (String.fromFloat val) of
                    Nothing ->
                        if
                            Set.member
                                (String.fromFloat val)
                                (Set.fromList [ "Infinity", "-Infinity", "NaN" ])
                        then
                            Expect.pass

                        else
                            checkParsing [ ( String.fromFloat val, EdnFloat val ) ]

                    Just _ ->
                        Expect.pass
        , fuzz int "fuzz integers" <|
            \val ->
                checkParsing [ ( String.fromInt val, EdnInt val ) ]
        , test "strings" <|
            \_ ->
                checkParsing
                    [ ( Json.Encode.encode 0 (Json.Encode.string ""), EdnString "" )
                    , ( Json.Encode.encode 0 (Json.Encode.string "\\"), EdnString "\\" )
                    , ( Json.Encode.encode 0 (Json.Encode.string "\n"), EdnString "\n" )
                    , ( "\"a string\\twith\\\\tttt\\\"'s\"", EdnString "a string\twith\\tttt\"'s" )
                    , ( """ "me & you \\u0026 them" """, EdnString "me & you & them" )
                    , ( "\"\\\\00\"", EdnString "\\00" )
                    , ( stringWithNewLine, EdnString "a\n  b\n" )
                    ]
        , test "nil" <|
            \_ ->
                checkParsing
                    [ ( "nil", EdnNil ) ]
        , test "keyword" <|
            \_ ->
                checkParsing
                    [ ( ":tony", EdnKeyword Nothing "tony" )
                    , ( ":tony/bradley", EdnKeyword (Just "tony") "bradley" )
                    ]
        , test "Maps" <|
            \_ ->
                checkParsing
                    [ ( """{:1 1 :2 2}"""
                      , EdnMap
                            [ ( EdnKeyword Nothing "1", EdnInt 1 )
                            , ( EdnKeyword Nothing "2", EdnInt 2 )
                            ]
                      )
                    ]
        , test "lists" <|
            \_ ->
                checkParsing
                    [ ( """(1 2 3)""", EdnList [ EdnInt 1, EdnInt 2, EdnInt 3 ] ) ]
        , test "sets" <|
            \_ ->
                checkParsing
                    [ ( """#{1 "2" false}""", EdnSet [ EdnInt 1, EdnString "2", EdnBool False ] )
                    , ( """#{{:1 1 :2 2} "test"}"""
                      , EdnSet
                            [ EdnMap
                                [ ( EdnKeyword Nothing "1", EdnInt 1 )
                                , ( EdnKeyword Nothing "2", EdnInt 2 )
                                ]
                            , EdnString "test"
                            ]
                      )
                    ]
        , test "vectors" <|
            \_ ->
                checkParsing
                    [ ( """[() #{} {}]"""
                      , EdnVector
                            (Array.fromList [ EdnList [], EdnSet [], EdnMap [] ])
                      )
                    ]
        , test "symbols" <|
            \_ ->
                checkParsing
                    [ ( "a", EdnSymbol "a" )
                    , ( "-1", EdnInt -1 )
                    , ( "+a1", EdnSymbol "+a1" )
                    , ( "a/a...", EdnSymbol "a/a..." )
                    , ( "a.a", EdnSymbol "a.a" )
                    , ( "a#a", EdnSymbol "a#a" )
                    , ( "-", EdnSymbol "-" )
                    , ( "+", EdnSymbol "+" )
                    , ( "$", EdnSymbol "$" )
                    , ( "%", EdnSymbol "%" )
                    , ( "&", EdnSymbol "&" )
                    , ( "*", EdnSymbol "*" )
                    , ( "/", EdnSymbol "/" )
                    , ( "**", EdnSymbol "**" )
                    ]
        , test "tags" <|
            \_ ->
                checkParsing
                    [ ( """#foo/bar "hello there" """, EdnTag "foo" "bar" (EdnString "hello there") )
                    , ( """#foo/bar [1 2 3] """
                      , EdnTag "foo"
                            "bar"
                            (EdnVector
                                (Array.fromList [ EdnInt 1, EdnInt 2, EdnInt 3 ])
                            )
                      )
                    , ( """#foo/bar #{1 2 3} """
                      , EdnTag "foo" "bar" (EdnSet [ EdnInt 1, EdnInt 2, EdnInt 3 ])
                      )
                    , ( """#foo/bar {:a 1 :b 2} """
                      , EdnTag "foo"
                            "bar"
                            (EdnMap
                                [ ( EdnKeyword Nothing "a", EdnInt 1 )
                                , ( EdnKeyword Nothing "b", EdnInt 2 )
                                ]
                            )
                      )
                    ]
        , test "example big edn document" <|
            \_ ->
                case run edn TestCases.casePlaylist of
                    Ok _ ->
                        Expect.pass

                    Err err ->
                        Expect.equal [] err
        , test "keyword errors" <|
            \_ ->
                Expect.all
                    [ \_ -> expectError (run edn ":")
                    , \_ -> expectError (run edn ":a/")
                    , \_ -> expectError (run edn ":/a")
                    , \_ -> expectError (run edn ":a/b/c")
                    ]
                    ()
        , test "taggedMap error" <|
            \_ ->
                Expect.all
                    [ \_ -> expectError (run edn "#foo{a1 1}")
                    ]
                    ()
        ]
