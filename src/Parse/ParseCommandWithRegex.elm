module Parse.ParseCommandWithRegex exposing (parse)

import DataStructure exposing (Command(..), getPayMethodFromString)
import Product exposing (Product, getProductById)
import Regex exposing (Regex)


commandRegex : Regex.Regex
commandRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "(add|pay|remove|end)\\-?([a-z\\d]+)?:?([\\d]+)?@([\\d]+)"


{-| parse a string to a tuple made of command and timestamp

    import Product exposing (Product)
    import DataStructure exposing (Command(..), PayMethod(..))

    parse "add-1:1@19700101235959"
    --> (Add (Product 1 10 False, Just 1), Just 19700101235959)

    parse "add-1@19700101235959"
    --> (Add (Product 1 10 False, Nothing), Just 19700101235959)

    parse "pay-cash@19700101235959"
    --> (Pay Cash, Just 19700101235959)

    parse "pay-check@19700101235959"
    --> (Pay Check, Just 19700101235959)

    parse "end@19700101235959"
    --> (End, Just 19700101235959)

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
            ( Unknown, Nothing )

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
                    case getProduct productId of
                        Nothing ->
                            ( Unknown, Nothing )

                        Just product ->
                            ( Add ( product, String.toInt quantity ), String.toInt timestamp )

                "remove" ->
                    case getProduct productId of
                        Nothing ->
                            ( Unknown, Nothing )

                        Just product ->
                            ( Remove ( product, String.toInt quantity ), String.toInt timestamp )

                "pay" ->
                    case getPayMethodFromString productId of
                        Nothing ->
                            ( Unknown, Nothing )

                        Just paymentMethod ->
                            ( Pay paymentMethod, String.toInt timestamp )

                "end" ->
                    ( End, String.toInt timestamp )

                _ ->
                    ( Unknown, Nothing )


takeElmentFromList : Int -> List a -> Maybe a
takeElmentFromList element list =
    list
        |> List.drop element
        |> List.head


getProduct : String -> Maybe Product
getProduct productId =
    String.toInt productId
        |> Maybe.andThen getProductById
