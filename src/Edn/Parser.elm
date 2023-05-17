module Edn.Parser exposing
    ( edn, run, Error
    , Definitions, applyDataReaders
    )

{-| A parser for the `Edn` union type, for use with [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/).
Supports full extensibility through custom data readers.


# Main Functionality

@docs edn, run, Error


# Extensibility

@docs Definitions, applyDataReaders

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Edn exposing (Edn(..))
import Edn.Parser.String
import Edn.Parser.Unicode exposing (unicodeEscape)
import Parser exposing (..)
import Set


{-| Convenience type alias for `elm/parser`'s `List DeadEnd` type
-}
type alias Error =
    List DeadEnd


{-| Convenience function for using `elm/parser` to run the `edn` parser in this module against
a given string
-}
run : String -> Result (List DeadEnd) Edn
run =
    Parser.run edn


{-| Parser for the Edn datatype. You likely just want to use `run` instead and don't need the parser
on its own like this.
-}
edn : Parser Edn
edn =
    succeed identity
        |= ednHelper
        |. end


ednHelper : Parser Edn
ednHelper =
    succeed identity
        |. ednWhitespace
        |= oneOf
            [ ednString
            , ednBool
            , ednNil
            , ednKeyword
            , backtrackable ednCharacter
            , backtrackable ednInt
            , backtrackable ednFloat
            , backtrackable ednSymbol
            , backtrackable ednTaggedMap
            , backtrackable (lazy (\_ -> ednTag))
            , lazy (\_ -> ednSet)
            , lazy (\_ -> ednVector)
            , lazy (\_ -> ednList)
            , lazy (\_ -> ednMap)
            ]
        |. ednWhitespace


ednWhitespace : Parser ()
ednWhitespace =
    chompWhile
        (\c ->
            False
                || (c == ' ')
                || (c == '\n')
                || (c == '\t')
                || (c == '\u{000D}')
                || (c == ',')
        )


optionalWhitespace : Parser ()
optionalWhitespace =
    oneOf
        [ ednWhitespace
        , succeed ()
        ]


ednCharacter : Parser Edn
ednCharacter =
    let
        charCodeParser : String -> Parser Char
        charCodeParser code =
            case Parser.run unicodeEscape ("u" ++ code) of
                Ok charCode ->
                    succeed charCode

                _ ->
                    problem ("Invalid character code: " ++ "\\u" ++ code)

        stringParser : String -> Parser Char
        stringParser charString =
            case String.uncons charString of
                Just ( c, rest ) ->
                    if String.length rest > 0 then
                        problem "Character cannot be longer than a single unit"

                    else
                        succeed c

                Nothing ->
                    problem "Empty character"
    in
    succeed EdnChar
        |. token "\\"
        |= oneOf
            [ map (always '\n') (keyword "newline")
            , map (always '\t') (keyword "tab")
            , map (always ' ') (keyword "space")
            , map (always '\u{000D}') (keyword "return")
            , backtrackable
                (succeed identity
                    |. token "u"
                    |= andThen charCodeParser
                        (variable
                            { start = Char.isHexDigit
                            , inner = Char.isHexDigit
                            , reserved = Set.fromList []
                            }
                        )
                )
            , andThen stringParser
                (succeed identity
                    |= variable
                        { start = Char.isAlphaNum
                        , inner = always False
                        , reserved = Set.fromList []
                        }
                )
            ]


ednKeyword : Parser Edn
ednKeyword =
    succeed EdnKeyword
        |. token ":"
        |= oneOf
            [ succeed Just
                |= backtrackable
                    (variable
                        { start = Char.isAlphaNum
                        , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
                        , reserved = Set.fromList [ "/" ]
                        }
                    )
                |. token "/"
            , succeed Nothing
            ]
        |= variable
            { start = Char.isAlphaNum
            , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
            , reserved = Set.empty
            }


ednMap : Parser Edn
ednMap =
    let
        pairUp : List ( Edn, Edn ) -> Int -> Array Edn -> Parser (List ( Edn, Edn ))
        pairUp pairs idx array =
            case
                Maybe.map2
                    Tuple.pair
                    (Array.get idx array)
                    (Array.get (idx + 1) array)
            of
                Nothing ->
                    succeed pairs

                Just pair ->
                    pairUp (pair :: pairs) (idx + 2) array

        pairCheck pairs =
            case modBy 2 (List.length pairs) of
                0 ->
                    succeed pairs

                _ ->
                    problem "Map has an uneven number of items"
    in
    ednSequence "{" "}"
        |> andThen pairCheck
        |> andThen (Array.fromList >> pairUp [] 0)
        |> map (List.reverse >> EdnMap)


ednList : Parser Edn
ednList =
    map EdnList (ednSequence "(" ")")


ednSet : Parser Edn
ednSet =
    map EdnSet (ednSequence "#{" "}")


ednVector : Parser Edn
ednVector =
    map (Array.fromList >> EdnVector) (ednSequence "[" "]")


ednSequence : String -> String -> Parser (List Edn)
ednSequence startToken endToken =
    succeed identity
        |. token startToken
        |. optionalWhitespace
        |= loop [] (ednSequenceHelper endToken)


ednSequenceHelper : String -> List Edn -> Parser (Step (List Edn) (List Edn))
ednSequenceHelper end items =
    oneOf
        [ backtrackable
            (succeed (\_ -> Done (List.reverse items))
                |. optionalWhitespace
                |= token end
            )
        , succeed (\item -> Loop (item :: items))
            |. ednWhitespace
            |= ednHelper
        ]


ednTaggedMap : Parser Edn
ednTaggedMap =
    andThen identity <|
        succeed
            (\ns val ->
                case val of
                    EdnMap ednMapVal ->
                        List.foldr
                            (\( k, v ) accResult ->
                                case ( accResult, k ) of
                                    ( Err _, _ ) ->
                                        accResult

                                    ( Ok acc, EdnKeyword Nothing tag_ ) ->
                                        Ok <| ( EdnKeyword (Just ns) tag_, v ) :: acc

                                    _ ->
                                        Err "Tagged map keys must be keywords"
                            )
                            (Ok [])
                            ednMapVal
                            |> Result.map EdnMap
                            |> (\r ->
                                    case r of
                                        Ok v ->
                                            succeed v

                                        Err e ->
                                            problem e
                               )

                    _ ->
                        problem "Tagged map must contain a map"
            )
            |. symbol "#"
            |= variable
                { start = Char.isAlphaNum
                , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
                , reserved = Set.fromList [ "#", "/" ]
                }
            |= ednMap


ednTag : Parser Edn
ednTag =
    succeed (\( ns, tag ) val -> EdnTag ns tag val)
        |= (succeed Tuple.pair
                |. symbol "#"
                |= variable
                    { start = Char.isAlphaNum
                    , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
                    , reserved = Set.fromList [ "#", "/" ]
                    }
                |. symbol "/"
                |= variable
                    { start = Char.isAlphaNum
                    , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
                    , reserved = Set.fromList [ "#", "/" ]
                    }
           )
        |= ednHelper


ednString : Parser Edn
ednString =
    map EdnString Edn.Parser.String.string


ednInt : Parser Edn
ednInt =
    oneOf
        [ succeed (\v -> EdnInt (v * -1))
            |. symbol "-"
            |= int
        , map EdnInt int
        ]


ednNil : Parser Edn
ednNil =
    map (always EdnNil) (keyword "nil")


ednBool : Parser Edn
ednBool =
    map EdnBool
        (oneOf
            [ map (always False) (keyword "false")
            , map (always True) (keyword "true")
            ]
        )


ednFloat : Parser Edn
ednFloat =
    oneOf
        [ succeed (\v -> EdnFloat (v * -1))
            |. symbol "-"
            |= float
        , map EdnFloat float
        ]


ednSymbol : Parser Edn
ednSymbol =
    oneOf
        [ map EdnSymbol (ednSymbolHelper False)
        , map (always <| EdnSymbol "/") (token "/")
        , oneOf
            (List.map
                (\c -> map (always (EdnSymbol <| String.fromChar c)) (keyword (String.fromChar c)))
                [ '.', '*', '+', '!', '-', '_', '?', '$', '%', '&', '=', '<', '>' ]
            )
        ]


ednSymbolHelper : Bool -> Parser String
ednSymbolHelper hasPrefix =
    let
        symbolCharacters =
            Set.fromList [ '.', '*', '+', '!', '-', '_', '?', '$', '%', '&', '=', '<', '>', '#' ]
    in
    backtrackable
        (succeed (\prefix suffix -> prefix ++ suffix)
            |= oneOf
                [ succeed (++)
                    |= variable
                        { start = \c -> Set.member c (Set.fromList [ '-', '+' ])
                        , inner = always False
                        , reserved = Set.empty
                        }
                    |= variable
                        { start = Char.isAlpha
                        , inner = \c -> Char.isAlphaNum c || Set.member c symbolCharacters
                        , reserved = Set.empty
                        }
                , variable
                    { start =
                        \c ->
                            Char.isAlpha c
                                || Set.member c
                                    (Set.diff symbolCharacters (Set.fromList [ '-', '+', '#' ]))
                    , inner = \c -> Char.isAlphaNum c || Set.member c symbolCharacters
                    , reserved = Set.empty
                    }
                ]
            |= oneOf
                [ token "/"
                    |> andThen
                        (\_ ->
                            if hasPrefix then
                                problem "No more than one prefix namespace allowed in symbol"

                            else
                                map (\v -> "/" ++ v) (ednSymbolHelper True)
                        )
                , succeed ""
                ]
        )


{-| Just as Clojure supports Edn extensibility through data\_readers.clj maps, this parser supports
supplying definitions to apply transformations to tagged values under a certain namespace. The
Definitions type is a tuple of the namespace to interpret, and a dictionary of transforms for
tags under that namespace.

Here is a short example of a "sum" tag under our application namespace that will sum up Edn integers
in a list following it

-}
type alias Definitions error =
    ( String, Dict String (Edn -> Result error Edn) )


{-| Apply a set of Defintions to an Edn value.
Here is a full example of how we might add extensibility to Edn data consumed by our application

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

    result =
        ednWithTags
            |> (Edn.Parser.run >> Result.mapError ParserError)
            |> Result.map (Edn.Parser.applyDataReaders testAppDefinitions)

-}
applyDataReaders : Definitions error -> Edn -> Result error Edn
applyDataReaders ( ns, defs ) ednVal =
    case ednVal of
        EdnTag ns_ tag expr ->
            case ( ns_ == ns, Dict.get tag defs ) of
                ( True, Just fn ) ->
                    applyDataReaders ( ns, defs ) expr
                        |> Result.andThen fn

                _ ->
                    Ok ednVal

        EdnList exprs ->
            List.foldr
                (\exp acc ->
                    Result.map2
                        (::)
                        (applyDataReaders ( ns, defs ) exp)
                        acc
                )
                (Result.Ok [])
                exprs
                |> Result.map EdnList

        EdnMap exprs ->
            List.foldr
                (\( kExp, vExp ) acc ->
                    Result.map2
                        (::)
                        (Result.map2
                            Tuple.pair
                            (applyDataReaders ( ns, defs ) kExp)
                            (applyDataReaders ( ns, defs ) vExp)
                        )
                        acc
                )
                (Result.Ok [])
                exprs
                |> Result.map EdnMap

        EdnSet exprs ->
            List.foldr
                (\exp acc ->
                    Result.map2
                        (::)
                        (applyDataReaders ( ns, defs ) exp)
                        acc
                )
                (Result.Ok [])
                exprs
                |> Result.map EdnSet

        EdnVector exprs ->
            Array.foldr
                (\exp acc ->
                    Result.map2
                        (::)
                        (applyDataReaders ( ns, defs ) exp)
                        acc
                )
                (Result.Ok [])
                exprs
                |> Result.map (Array.fromList >> EdnVector)

        _ ->
            Ok ednVal
