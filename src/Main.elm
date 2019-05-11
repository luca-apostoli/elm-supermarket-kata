module Main exposing (start, startRecursive)

import DataStructure exposing (CheckoutError, CheckoutProduct, CheckoutSuccess, Command(..), PayMethod(..))
import Execute.ExecuteCommands exposing (executeCommand)
import Parse.ParseCommand exposing (convertStringToCommand)
import Parse.ParseCommandWithRegex exposing (parse)


{-| Start execution and return results

    import DataStructure exposing (CheckoutError, CheckoutProduct, CheckoutSuccess, Command(..), PayMethod(..))

    start ["add-1:1@19700101235959", "pay-cash@19700101235959", "end@19700101235959"]
    --> Ok { def = "success", extraFee = 0, paymentMethod = Cash, products = [{ id = 1, quantity = 1, total = 10 }], total = 10, vat = 2.2 }

    start ["add-1:3@19700101235959", "remove-1:2@19700101235959", "pay-cash@19700101235959", "end@19700101235959"]
    --> Ok { def = "success", extraFee = 0, paymentMethod = Cash, products = [{ id = 1, quantity = 1, total = 10 }], total = 10, vat = 2.2 }

    start ["add-1:3@19700101235959", "remove-1:2@19700101235959", "pay-check@19700101235959", "end@19700101235959"]
    --> Ok { def = "success", extraFee = 2, paymentMethod = Check, products = [{ id = 1, quantity = 1, total = 10 }], total = 12, vat = 2.2 }

    start ["remove-1:2@19700101235953", "add-1:3@19700101235951", "end@19700101235959", "pay-check@19700101235955"]
    --> Ok { def = "success", extraFee = 2, paymentMethod = Check, products = [{ id = 1, quantity = 1, total = 10 }], total = 12, vat = 2.2 }

    start ["remove-1@19700101235953", "add-1:3@19700101235951", "end@19700101235959", "pay-check@19700101235955"]
    --> Ok { def = "success", extraFee = 4, paymentMethod = Check, products = [{ id = 1, quantity = 2, total = 20 }], total = 24, vat = 4.4 }

    start ["remove-1:2@19700101235953", "add19700101235951", "end@19700101235959", "pay-check@19700101235955"]
    --> Err { def = "error", errors = [] }

-}
start : List String -> Result CheckoutError CheckoutSuccess
start commandList =
    commandList
        |> List.map convertStringToCommand
        |> List.sortBy (Maybe.withDefault 0 << Tuple.second)
        |> List.foldl (getResultFromCommand << Tuple.first) (Ok initCheckout)


getResultFromCommand : Command -> Result CheckoutError CheckoutSuccess -> Result CheckoutError CheckoutSuccess
getResultFromCommand command resultCheckout =
    resultCheckout
        |> Result.andThen (executeCommand command)


initCheckout : CheckoutSuccess
initCheckout =
    { def = "success", total = 0, vat = 0, extraFee = 0, paymentMethod = Cash, products = [] }



-- RECURSIVE IMPLEMENTATION


startRecursive : List String -> Result CheckoutError CheckoutSuccess
startRecursive commandlist =
    let
        checkout =
            initCheckout

        listCmd =
            commandlist
                |> List.map parse
                |> List.sortBy (Maybe.withDefault 0 << Tuple.second)
                |> List.map Tuple.first
    in
    executeAllCommandsRecursive listCmd checkout


executeAllCommandsRecursive : List Command -> CheckoutSuccess -> Result CheckoutError CheckoutSuccess
executeAllCommandsRecursive commands checkout =
    if List.isEmpty commands then
        Ok checkout

    else
        let
            updated =
                executeCommand <| Maybe.withDefault End <| List.head commands
        in
        case updated checkout of
            Err err ->
                Err err

            Ok checkoutSuccess ->
                executeAllCommandsRecursive (Maybe.withDefault [] <| List.tail commands) checkoutSuccess



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
