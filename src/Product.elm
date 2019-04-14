module Product exposing (Product, getProductById)

import Json.Decode as JD


type alias Product =
    { id : Int
    , price : Float
    , isFood : Bool
    }


getProductById : Int -> Maybe Product
getProductById productId =
    let
        productList =
            loadProductFromList
    in
    productList
        |> List.filter (\el -> el.id == productId)
        |> List.head


decodeProduct : JD.Decoder Product
decodeProduct =
    JD.map3 Product
        (JD.field "id" JD.int)
        (JD.field "price" JD.float)
        (JD.field "isFood" JD.bool)


decodeProductList : JD.Decoder (List Product)
decodeProductList =
    JD.list decodeProduct


loadProductFromList : List Product
loadProductFromList =
    let
        productJson =
            """
            [
              {
                "id": 1,
                "price": 10,
                "isFood": false
              },
              {
                "id": 2,
                "price": 10,
                "isFood": false
              }
            ]
            """
    in
    JD.decodeString decodeProductList productJson
        |> Result.withDefault []
