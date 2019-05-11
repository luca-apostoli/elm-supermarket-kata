module DataStructure exposing (CheckoutError, CheckoutProduct, CheckoutSuccess, Command(..), PayMethod(..), getPayMethodFromString)

import Product exposing (Product)


type Command
    = Unknown
    | Add ( Product, Maybe Int )
    | Remove ( Product, Maybe Int )
    | Pay PayMethod
    | End


type PayMethod
    = Cash
    | Check


type alias CheckoutProduct =
    { id : Int
    , quantity : Int
    , total : Float
    }


type alias CheckoutError =
    { def : String
    , errors : List String
    }


type alias CheckoutSuccess =
    { def : String
    , total : Float
    , vat : Float
    , extraFee : Float
    , paymentMethod : PayMethod
    , products : List CheckoutProduct
    }


getPayMethodFromString : String -> Maybe PayMethod
getPayMethodFromString payMethod =
    case payMethod of
        "check" ->
            Just Check

        "cash" ->
            Just Cash

        _ ->
            Nothing
