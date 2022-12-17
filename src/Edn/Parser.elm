module Edn.Parser exposing (..)

import Array exposing (Array)
import Edn exposing (Edn(..))
import Edn.Parser.String
import Parser exposing (..)
import Set


edn : Parser Edn
edn =
    succeed identity
        |. ednWhitespace
        |= oneOf
            [ ednString
            , ednInt
            , ednBool
            , ednNil
            , ednFloat
            , ednKeyword
            , ednCharacter
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


ednCharacter : Parser Edn
ednCharacter =
    let
        charCodeParser : String -> Parser Char
        charCodeParser code =
            case run int ("0x" ++ code) of
                Ok charCode ->
                    succeed (Char.fromCode charCode)

                _ ->
                    problem "invalid character code"

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
                            { start = Char.isAlphaNum
                            , inner = Char.isAlphaNum
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
            [ map Just
                (backtrackable
                    (variable
                        { start = Char.isAlphaNum
                        , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
                        , reserved = Set.empty
                        }
                    )
                )
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
    andThen (Array.fromList >> pairUp [] 0) (ednSequence "(" ")")
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
        |= loop [] (ednSequenceHelper endToken)


ednSequenceHelper : String -> List Edn -> Parser (Step (List Edn) (List Edn))
ednSequenceHelper end items =
    oneOf
        [ map (always <| Done (List.reverse items)) (token end)
        , map (always <| Loop items) ednWhitespace
        , map (\item -> Loop (item :: items)) edn
        ]


ednTag : Parser Edn
ednTag =
    succeed EdnTag
        |= (succeed Tuple.pair
                |. symbol "#"
                |= variable
                    { start = Char.isAlphaNum
                    , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
                    , reserved = Set.empty
                    }
                |. symbol "/"
                |= variable
                    { start = Char.isAlphaNum
                    , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
                    , reserved = Set.empty
                    }
           )
        |= edn


ednString : Parser Edn
ednString =
    map EdnString Edn.Parser.String.string


ednInt : Parser Edn
ednInt =
    map EdnInt int


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
    map EdnFloat float
