module ParseCommandWithRegex exposing (parse)

import DataStructure exposing (Command(..), getPayMethodFromString)
import Product exposing (Product, getProductById)
import Regex exposing (Regex)


commandRegex : Regex.Regex
commandRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "(add|pay|remove|end)\\-?([a-z\\d]+)?:?([\\d]+)?@([\\d]+)"


{-| parse a string to a tuple made of command and timestamp

    import Product exposing (getProductById)
    import DataStructure exposing (Command(..))

    parse "add-1:1@19700101235959"
    --> (Add (getProductById 1, Just 1), Just 19700101235959)

    parse "add-1@19700101235959"
    --> (Add (getProductById 1, Nothing), Just 19700101235959)

-}
parse : String -> ( Command, Maybe Int )
parse commandString =
    findCommandData commandString
        |> convertMatchesToCommand


findCommandData : String -> Maybe Regex.Match
findCommandData command =
    Regex.find commandRegex command
        |> List.head


convertMatchesToCommand : Maybe Regex.Match -> ( Command, Maybe Int )
convertMatchesToCommand matches =
    case matches of
        Nothing ->
            ( End, Nothing )

        Just match ->
            let
                commandType =
                    match |> .submatches |> takeElmentFromList 0 |> Maybe.withDefault (Just "") |> Maybe.withDefault ""

                productId =
                    match |> .submatches |> takeElmentFromList 1 |> Maybe.withDefault (Just "") |> Maybe.withDefault ""

                quantity =
                    match |> .submatches |> takeElmentFromList 2 |> Maybe.withDefault (Just "") |> Maybe.withDefault ""

                timestamp =
                    match |> .submatches |> takeElmentFromList 3 |> Maybe.withDefault (Just "") |> Maybe.withDefault ""
            in
            case commandType of
                "add" ->
                    ( Add ( getProduct productId, String.toInt quantity ), String.toInt timestamp )

                "remove" ->
                    ( Remove ( getProduct productId, String.toInt quantity ), String.toInt timestamp )

                "pay" ->
                    ( Pay (getPayMethodFromString productId), String.toInt timestamp )

                _ ->
                    ( End, String.toInt timestamp )


takeElmentFromList : Int -> List a -> Maybe a
takeElmentFromList element list =
    list
        |> List.drop element
        |> List.head


getProduct : String -> Maybe Product
getProduct productId =
    String.toInt productId
        |> Maybe.andThen getProductById
