module Edn.Decode exposing (..)

import Array exposing (Array)
import Edn exposing (Edn(..))
import Set exposing (Set)


showError : Edn -> Edn -> Context -> String
showError expected found ctx =
    "Expected: "
        ++ showType expected
        ++ " - Found: "
        ++ showType found
        ++ (case ctx.path of
                Just path ->
                    " - Path: " ++ String.join ", " path

                Nothing ->
                    ""
           )


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
    map fn decoderA
        |> andMap decoderB


map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 fn decoderA decoderB decoderC =
    map fn decoderA
        |> andMap decoderB
        |> andMap decoderC


map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 fn decoderA decoderB decoderC decoderD =
    map fn decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD


map5 : (a -> b -> c -> d -> e -> f) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f
map5 fn decoderA decoderB decoderC decoderD decoderE =
    map fn decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD
        |> andMap decoderE


map6 : (a -> b -> c -> d -> e -> f -> g) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g
map6 fn decoderA decoderB decoderC decoderD decoderE decoderF =
    map fn decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD
        |> andMap decoderE
        |> andMap decoderF


map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h
map7 fn decoderA decoderB decoderC decoderD decoderE decoderF decoderG =
    map fn decoderA
        |> andMap decoderB
        |> andMap decoderC
        |> andMap decoderD
        |> andMap decoderE
        |> andMap decoderF
        |> andMap decoderG


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
