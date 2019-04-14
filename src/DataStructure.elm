module DataStructure exposing (CheckoutProduct, Command(..), PayMethod(..), getPayMethodFromString)

import Product exposing (Product)


type Command
    = Add ( Maybe Product, Maybe Int )
    | Remove ( Maybe Product, Maybe Int )
    | Pay (Maybe PayMethod)
    | End


type alias CheckoutProduct =
    { id : Int
    , quantity : Int
    , total : Float
    }


type PayMethod
    = Cash
    | Check


getPayMethodFromString : String -> Maybe PayMethod
getPayMethodFromString payMethod =
    case payMethod of
        "check" ->
            Just Check

        "cash" ->
            Just Cash

        _ ->
            Nothing
