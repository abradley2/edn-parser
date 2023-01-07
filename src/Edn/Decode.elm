module Edn.Decode exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Edn exposing (Edn(..))
import Set exposing (Set)


formatCtx : Context -> String
formatCtx ctx =
    case ctx.path of
        Just path ->
            " - Path: " ++ String.join ", " path

        Nothing ->
            ""


showError : Edn -> Edn -> Context -> String
showError expected found ctx =
    "Expected: "
        ++ showType expected
        ++ " - Found: "
        ++ showType found
        ++ formatCtx ctx


showType : Edn -> String
showType value =
    case value of
        EdnString _ ->
            "String"

        EdnVariable _ ->
            "Variable"

        EdnKeyword _ _ ->
            "Keyword"

        EdnList _ ->
            "List"

        EdnVector _ ->
            "Vector"

        EdnMap _ ->
            "Map"

        EdnSet _ ->
            "Set"

        EdnNil ->
            "Nil"

        EdnBool _ ->
            "Bool"

        EdnInt _ ->
            "Int"

        EdnFloat _ ->
            "Float"

        EdnChar _ ->
            "Char"

        EdnTag _ _ _ ->
            "Tag"

        EdnSymbol _ ->
            "Symbol"


type alias Context =
    { path : Maybe (List String)
    }


type alias Decoder a =
    Context -> Edn -> Result String a


withCtx : String -> Decoder a -> Decoder a
withCtx key decoder ctx edn =
    case
        decoder
            { ctx
                | path =
                    ctx.path
                        |> Maybe.map (\path -> path ++ [ key ])
                        |> Maybe.withDefault [ key ]
                        |> Just
            }
            edn
    of
        Ok value ->
            Ok value

        Err err ->
            Err err


find : (a -> Bool) -> List a -> Maybe a
find check items =
    case items of
        [] ->
            Nothing

        x :: xs ->
            if check x then
                Just x

            else
                find check xs


atKey : Edn -> Decoder Edn
atKey key =
    ednMap raw raw
        |> andThen
            (\rawList ->
                case find (Tuple.first >> (==) key) rawList of
                    Just rawValue ->
                        succeed <| Tuple.second rawValue

                    Nothing ->
                        fail <| "Key " ++ show key ++ " not found"
            )
        |> withCtx (show key)


atIndex : Int -> Decoder Edn
atIndex index =
    vector raw
        |> andThen
            (\array ->
                case Array.get index array of
                    Just rawValue ->
                        succeed rawValue

                    Nothing ->
                        fail <| "Index " ++ String.fromInt index ++ " out of bounds "
            )
        |> withCtx (String.fromInt index)


succeed : a -> Decoder a
succeed value _ _ =
    Ok value


fail : String -> Decoder a
fail error _ _ =
    Err error


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen fn decoder ctx edn =
    case decoder ctx edn of
        Ok value ->
            fn value ctx edn

        Err err ->
            Err err


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap decoder apDecoder ctx edn =
    case decoder ctx edn of
        Ok value ->
            map (\fn -> fn value) apDecoder ctx edn

        Err err ->
            Err err


map : (a -> b) -> Decoder a -> Decoder b
map fn decoder ctx edn =
    Result.map
        fn
        (decoder ctx edn)


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 fn decoderA decoderB =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)


map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 fn decoderA decoderB decoderC =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)


map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 fn decoderA decoderB decoderC decoderD =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)


map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 fn decoderA decoderB decoderC decoderD decoderE =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)
        |> andMap (withCtx "Field 5" decoderE)


map6 : (a -> b -> c -> d -> e -> f -> g) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g
map6 fn decoderA decoderB decoderC decoderD decoderE decoderF =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)
        |> andMap (withCtx "Field 5" decoderE)
        |> andMap (withCtx "Field 6" decoderF)


map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h
map7 fn decoderA decoderB decoderC decoderD decoderE decoderF decoderG =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)
        |> andMap (withCtx "Field 5" decoderE)
        |> andMap (withCtx "Field 6" decoderF)
        |> andMap (withCtx "Field 7" decoderG)


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder i
map8 fn decoderA decoderB decoderC decoderD decoderE decoderF decoderG decoderH =
    map fn (withCtx "Field 1" decoderA)
        |> andMap (withCtx "Field 2" decoderB)
        |> andMap (withCtx "Field 3" decoderC)
        |> andMap (withCtx "Field 4" decoderD)
        |> andMap (withCtx "Field 5" decoderE)
        |> andMap (withCtx "Field 6" decoderF)
        |> andMap (withCtx "Field 7" decoderG)
        |> andMap (withCtx "Field 8" decoderH)


decode : Decoder a -> Edn -> Result String a
decode decoder edn =
    decoder { path = Nothing } edn


raw : Decoder Edn
raw _ value =
    Ok value


list : Decoder a -> Decoder (List a)
list decoder ctx value =
    case value of
        EdnList listValue ->
            listHelper decoder ctx 0 listValue []

        _ ->
            Err <| showError (EdnList []) value ctx


listHelper : Decoder a -> Context -> Int -> List Edn -> List a -> Result String (List a)
listHelper decoder ctx idx values decodedValues =
    case values of
        value :: next ->
            case
                decoder
                    { ctx
                        | path =
                            Maybe.map (\path -> path ++ [ String.fromInt idx ])
                                ctx.path
                                |> Maybe.withDefault [ String.fromInt idx ]
                                |> Just
                    }
                    value
            of
                Ok decodedValue ->
                    listHelper decoder ctx (idx + 1) next (decodedValue :: decodedValues)

                Err err ->
                    Err err

        [] ->
            Ok (List.reverse decodedValues)


set : Decoder a -> Decoder (List a)
set decoder ctx value =
    case value of
        EdnSet setValue ->
            decode (list decoder) (EdnList setValue)

        _ ->
            Err <| showError (EdnSet []) value ctx


setBy : Decoder a -> (a -> comparable) -> Decoder (Set comparable)
setBy decoder toComparable =
    map (List.map toComparable >> Set.fromList) (set decoder)


ednMap : Decoder key -> Decoder value -> Decoder (List ( key, value ))
ednMap keyDecoder valueDecoder ctx value =
    case value of
        EdnMap mapValue ->
            decode (list (map2 Tuple.pair keyDecoder valueDecoder)) (EdnMap mapValue)

        _ ->
            Err <| showError (EdnMap []) value ctx


ednMapBy : Decoder key -> Decoder value -> (key -> comparable) -> Decoder (Dict comparable value)
ednMapBy keyDecoder valueDecoder toComparable =
    map
        (List.map (\( key, value ) -> ( toComparable key, value )) >> Dict.fromList)
        (ednMap keyDecoder valueDecoder)


vector : Decoder a -> Decoder (Array a)
vector decoder ctx value =
    case value of
        EdnSet setValue ->
            decode (list decoder) (EdnList setValue)
                |> Result.map Array.fromList

        _ ->
            Err <| showError (EdnSet []) value ctx


int : Decoder Int
int ctx value =
    case value of
        EdnInt intValue ->
            Ok intValue

        _ ->
            Err <| showError (EdnInt 0) value ctx


float : Decoder Float
float ctx value =
    case value of
        EdnFloat floatValue ->
            Ok floatValue

        _ ->
            Err <| showError (EdnFloat 0.0) value ctx


string : Decoder String
string ctx value =
    case value of
        EdnString stringValue ->
            Ok stringValue

        _ ->
            Err <| showError (EdnString "") value ctx


char : Decoder Char
char ctx value =
    case value of
        EdnChar charValue ->
            Ok charValue

        _ ->
            Err <| showError (EdnChar 'x') value ctx


bool : Decoder Bool
bool ctx value =
    case value of
        EdnBool boolValue ->
            Ok boolValue

        _ ->
            Err <| showError (EdnBool False) value ctx


nil : Decoder ()
nil ctx value =
    case value of
        EdnNil ->
            Ok ()

        _ ->
            Err <| showError EdnNil value ctx


oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    oneOfHelper decoders Nothing


oneOfHelper : List (Decoder a) -> Maybe String -> Decoder a
oneOfHelper decoders maybeErr ctx value =
    case decoders of
        [] ->
            case maybeErr of
                Just err ->
                    Err err

                Nothing ->
                    Err ("No Match " ++ formatCtx ctx)

        decoder :: next ->
            case decoder ctx value of
                Ok result ->
                    Ok result

                Err err ->
                    oneOfHelper next (Just err) ctx value


optional : Decoder a -> Decoder (Maybe a)
optional decoder =
    oneOf [ map Just decoder, map (always Nothing) nil ]


try : Decoder a -> Decoder (Result String a)
try decoder ctx value =
    case decoder ctx value of
        Ok result ->
            Ok (Ok result)

        Err err ->
            Ok (Err err)


show : Edn -> String
show edn =
    case edn of
        EdnString s ->
            "\"" ++ s ++ "\""

        EdnVariable s ->
            "#" ++ s

        EdnKeyword ns s ->
            case ns of
                Just nsval ->
                    ":" ++ nsval ++ "/" ++ s

                Nothing ->
                    ":" ++ s

        EdnList l ->
            "(" ++ String.join " " (List.map show l) ++ ")"

        EdnVector a ->
            "[" ++ String.join " " (Array.toList (Array.map show a)) ++ "]"

        EdnMap l ->
            "{" ++ String.join " " (List.map (\( k, v ) -> show k ++ " " ++ show v) l) ++ "}"

        EdnSet l ->
            "#{" ++ String.join " " (List.map show l) ++ "}"

        EdnNil ->
            "nil"

        EdnBool b ->
            if b then
                "true"

            else
                "false"

        EdnInt i ->
            String.fromInt i

        EdnFloat f ->
            String.fromFloat f

        EdnChar c ->
            "\\" ++ String.fromChar c

        EdnTag ns tag v ->
            "#" ++ ns ++ "/" ++ tag ++ " " ++ show v

        EdnSymbol s ->
            s
