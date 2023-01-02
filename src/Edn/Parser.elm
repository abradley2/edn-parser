module Edn.Parser exposing (..)

{-| A parser for the `Edn` union type, for use with [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/)


# Main Functionality

@docs edn

-}

import Array exposing (Array)
import Edn exposing (Edn(..))
import Edn.Parser.String
import Edn.Parser.Unicode exposing (unicodeEscape)
import Parser exposing (..)
import Set


edn : Parser Edn
edn =
    succeed identity
        |. ednWhitespace
        |= oneOf
            [ ednString
            , ednBool
            , ednNil
            , ednKeyword
            , ednCharacter
            , backtrackable ednInt
            , ednFloat
            , lazy (\_ -> ednTag)
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
            case run unicodeEscape ("u" ++ code) of
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
    succeed (\namespace kw -> EdnKeyword ( namespace, kw ))
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
                    problem "Map has an uneven number of items"

                Just pair ->
                    pairUp (pair :: pairs) (idx + 2) array
    in
    andThen (Array.fromList >> pairUp [] 0) (ednSequence "{" "}")
        |> map EdnMap


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
            |= edn
        ]


ednTag : Parser Edn
ednTag =
    succeed EdnTag
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
        |= edn


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
