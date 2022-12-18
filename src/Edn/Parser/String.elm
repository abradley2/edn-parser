module Edn.Parser.String exposing (codeToChar, string, unicode)

import Edn.Parser.Unicode exposing (unicodeEscape)
import Parser exposing (..)
import Set



{- Handling for strings which may have escaped characters and escaped escaped characters!
   This is pretty tricky so I just ripped this from the elm/parser examples
   https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm
-}


string : Parser String
string =
    succeed identity
        |. token "\""
        |= loop [] stringHelp


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: revChunks))
            |. token "\\"
            |= oneOf
                [ map (\_ -> "\n") (token "n")
                , map (\_ -> "\t") (token "t")
                , map (\_ -> "\\") (token "\\")
                , map (\_ -> "\u{000D}") (token "r")
                , succeed String.fromChar
                    |. token "u"
                    |= (variable
                            { start = Char.isHexDigit
                            , inner = Char.isHexDigit
                            , reserved = Set.empty
                            }
                            |> andThen
                                (\hexStr ->
                                    case run unicodeEscape ("u" ++ hexStr) of
                                        Ok uniChar ->
                                            succeed uniChar

                                        Err _ ->
                                            problem ("Invalid unicode found: " ++ "\\u" ++ hexStr)
                                )
                       )
                ]
        , token "\""
            |> map (\_ -> Done (String.join "" (List.reverse revChunks)))
        , chompWhile isUninteresting
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'


unicode : Parser Char
unicode =
    getChompedString (chompWhile Char.isHexDigit)
        |> andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        succeed (Char.fromCode code)

    else
        problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)
