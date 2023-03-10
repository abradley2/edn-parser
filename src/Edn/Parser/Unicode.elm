module Edn.Parser.Unicode exposing (unicodeEscape)

import Parser exposing (..)
import Set


{-| Adapted from:
<https://github.com/robx/elm-edn/blob/9758e395ecb06b652786e2c3f4c10cc2a58d4308/src/Parser.elm>

Copyright (c) 2018-present, Robert Vollmert
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  - Neither the name of the {organization} nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}
unicodeEscape : Parser Char
unicodeEscape =
    let
        onehex c =
            case Char.toLower c of
                '0' ->
                    0

                '1' ->
                    1

                '2' ->
                    2

                '3' ->
                    3

                '4' ->
                    4

                '5' ->
                    5

                '6' ->
                    6

                '7' ->
                    7

                '8' ->
                    8

                '9' ->
                    9

                'a' ->
                    10

                'b' ->
                    11

                'c' ->
                    12

                'd' ->
                    13

                'e' ->
                    14

                'f' ->
                    15

                _ ->
                    0

        hexrev s =
            case String.uncons s of
                Just ( c, rest ) ->
                    onehex c + 16 * hexrev rest

                Nothing ->
                    0
    in
    succeed (Char.fromCode << hexrev << String.reverse)
        |. symbol "u"
        |= variable
            { start = Char.isHexDigit
            , inner = Char.isHexDigit
            , reserved = Set.empty
            }
