module Execute.ExecuteCommands exposing (executeCommand)

import DataStructure exposing (CheckoutError, CheckoutProduct, CheckoutSuccess, Command(..), PayMethod(..))
import Product exposing (Product)


executeCommand : Command -> CheckoutSuccess -> Result CheckoutError CheckoutSuccess
executeCommand command currentCheckout =
    case command of
        Add ( product, Nothing ) ->
            addProduct product 1 currentCheckout

        Add ( product, Just quantity ) ->
            addProduct product quantity currentCheckout

        Remove ( product, Nothing ) ->
            removeProduct product 1 currentCheckout

        Remove ( product, Just quantity ) ->
            removeProduct product quantity currentCheckout

        Pay method ->
            pay method currentCheckout

        End ->
            Ok currentCheckout

        Unknown ->
            Err { def = "error", errors = [] }


addProduct : Product -> Int -> CheckoutSuccess -> Result CheckoutError CheckoutSuccess
addProduct product quantity currentCheckout =
    if quantity > 0 then
        Ok
            { currentCheckout
                | products = List.append currentCheckout.products [ convertProductToCheckoutProduct product quantity ]
                , vat = currentCheckout.vat + calculateVat product quantity
            }

    else
        Err { def = "error", errors = [] }


convertProductToCheckoutProduct : Product -> Int -> CheckoutProduct
convertProductToCheckoutProduct product quantity =
    CheckoutProduct product.id quantity (toFloat quantity * product.price)


removeProduct : Product -> Int -> CheckoutSuccess -> Result CheckoutError CheckoutSuccess
removeProduct product quantity currentCheckout =
    if quantity > 0 then
        let
            productToRemove =
                convertProductToCheckoutProduct product quantity
        in
        Ok
            { currentCheckout
                | products =
                    List.map (decrementProduct productToRemove) currentCheckout.products
                        |> List.filter (\el -> el.id /= 0)
                , vat = currentCheckout.vat - calculateVat product quantity
            }

    else
        Err { def = "error", errors = [] }


decrementProduct : CheckoutProduct -> CheckoutProduct -> CheckoutProduct
decrementProduct productToRemove productToCompare =
    if productToRemove.id == productToCompare.id then
        if productToRemove.quantity < productToCompare.quantity then
            { productToCompare
                | quantity = productToCompare.quantity - productToRemove.quantity
                , total = productToCompare.total - productToRemove.total
            }

        else
            { productToCompare | id = 0, total = 0, quantity = 0 }

    else
        productToCompare


pay : PayMethod -> CheckoutSuccess -> Result CheckoutError CheckoutSuccess
pay method currentCheckout =
    let
        netTotal =
            List.foldl ((+) << .total) 0 currentCheckout.products

        extraFee =
            getExtraFeeFromPaymentMethod method netTotal

        total =
            netTotal + extraFee
    in
    Ok { currentCheckout | total = total, paymentMethod = method, extraFee = extraFee }


calculateVat : Product -> Int -> Float
calculateVat product quantity =
    getVatFromProduct product * toFloat quantity


getVatFromProduct : Product -> Float
getVatFromProduct product =
    if product.isFood == True then
        product.price * 10 / 100

    else
        product.price * 22 / 100


getExtraFeeFromPaymentMethod : PayMethod -> Float -> Float
getExtraFeeFromPaymentMethod method netTotal =
    case method of
        Check ->
            netTotal * 0.2

        Cash ->
            netTotal * 0.0
