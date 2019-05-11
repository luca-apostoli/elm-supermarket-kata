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
