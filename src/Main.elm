module Main exposing (start)

import DataStructure exposing (CheckoutProduct, Command(..), PayMethod(..))
import ParseCommandWithRegex exposing (parse)
import Product exposing (Product)


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


start : List String -> Result CheckoutError CheckoutSuccess
start commandlist =
    let
        checkout =
            initCheckout

        listCmd =
            commandlist
                |> List.map parse
                |> List.sortBy (Maybe.withDefault 0 << Tuple.second)
                |> List.map Tuple.first
    in
    executeAllCommands listCmd checkout


executeAllCommands : List Command -> CheckoutSuccess -> Result CheckoutError CheckoutSuccess
executeAllCommands commands checkout =
    if List.isEmpty commands then
        Ok checkout

    else
        let
            updated =
                executeCommand checkout <| Maybe.withDefault End <| List.head commands
        in
        case updated of
            Err err ->
                Err err

            Ok checkoutSuccess ->
                executeAllCommands (Maybe.withDefault [] <| List.tail commands) checkoutSuccess


initCheckout : CheckoutSuccess
initCheckout =
    { def = "", total = 0, vat = 0, extraFee = 0, paymentMethod = Cash, products = [] }


executeCommand : CheckoutSuccess -> Command -> Result CheckoutError CheckoutSuccess
executeCommand currentCheckout command =
    case command of
        Add ( Just product, Nothing ) ->
            addProduct product 1 currentCheckout

        Add ( Just product, Just quantity ) ->
            addProduct product quantity currentCheckout

        Add ( Nothing, _ ) ->
            Err { def = "error", errors = [] }

        Remove ( Just product, Nothing ) ->
            removeProduct product 1 currentCheckout

        Remove ( Just product, Just quantity ) ->
            removeProduct product quantity currentCheckout

        Remove ( Nothing, _ ) ->
            Err { def = "error", errors = [] }

        Pay (Just method) ->
            pay method currentCheckout

        Pay Nothing ->
            Err { def = "error", errors = [] }

        End ->
            Ok currentCheckout


addProduct : Product -> Int -> CheckoutSuccess -> Result CheckoutError CheckoutSuccess
addProduct product quantity currentCheckout =
    if quantity > 0 then
        Ok
            { currentCheckout
                | products = List.append currentCheckout.products [ convertProductToCheckoutProduct product quantity ]
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
        total =
            List.foldl ((+) << .total) 0 currentCheckout.products
    in
    Ok { currentCheckout | total = total, paymentMethod = method }



{---

# Supermarket 70

*Constraint*

The 70s Compiler - http://kata-log.rocks/the-70s-compiler

##  STEP 1

Bisogna svilippare un registratore di cassa in grado di acquisire dei comandi e fornire il risultato

I comandi base sono:

- `add-{productId}@19700101235959` add product with `{productId}`
- `add-{productId}:{n}@19700101235959` add n product with `{productId}`
- `remove-{productId}@19700101235959` remove product with `{productId}`
- `remove-{productId}:{n}@19700101235959` remove n product with `{productId}`
- `pay-check@19700101235959` paga con assegno
- `pay-cash@19700101235959` paga con contanti
- `end@19700101235959`

I prodotti di tipo alimentare sono tassati al 10%, tutti gli altri al 22%\
Il cliente può decidere in qualsiasi momento che metodo di pagamento usare ed eventualmente cambiarlo\
Se viene scelto come metodo di pagamento assegno dovranno essere aggiunte delle spese di gestione di 0.2% sul totale\
Gli eventi sono processati tramite coda quindi l'ordine di arrivo non è garantito


Una volta ricevuto il comando `end` bisogna stampare il risultato che dovrà essere del tipo:

```
type result<E> =
| {
    type: error
    errors: E[]
}
| {
    type: success
    total: number
    vat: number
    extraFee: number
    paymentMethod: 'check' | 'cash'
    products: {id: number, quantity: number, total: number}[]
}
```

---}
