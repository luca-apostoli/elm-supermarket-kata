module Parse.ParseCommand exposing (convertStringToCommand)

import Basics exposing (Never)
import DataStructure exposing (Command(..), PayMethod(..), getPayMethodFromString)
import Parser exposing ((|.), (|=), Parser, chompWhile, end, getChompedString, int, keyword, map, oneOf, run, succeed, symbol)
import Product exposing (getProductById)


type alias CommandStructure =
    { cmd : String
    , secondArgument : Maybe String
    , quantity : Maybe Int
    , timestamp : Int
    }


parseCommandStructure : Parser CommandStructure
parseCommandStructure =
    oneOf
        [ succeed CommandStructure
            |= map (\_ -> "end") (keyword "end")
            |= succeed Nothing
            |= succeed Nothing
            |. symbol "@"
            |= int
            |. end
        , succeed CommandStructure
            |= parseCommand
            |. symbol "-"
            |= parseSecondArgumentOrNothing
            |= parseQuantityOrNothing
            |. symbol "@"
            |= int
            |. end
        ]


parseCommand : Parser String
parseCommand =
    oneOf
        [ map (\_ -> "add") (keyword "add")
        , map (\_ -> "remove") (keyword "remove")
        , map (\_ -> "pay") (keyword "pay")
        ]


parseSecondArgumentOrNothing : Parser (Maybe String)
parseSecondArgumentOrNothing =
    succeed Just
        |= (getChompedString <|
                chompWhile (\c -> Char.isAlphaNum c && c /= '@' && c /= ':')
           )


parseQuantityOrNothing : Parser (Maybe Int)
parseQuantityOrNothing =
    oneOf
        [ map (\v -> v |> String.replace ":" "" |> String.toInt) <|
            getChompedString <|
                succeed ()
                    |. symbol ":"
                    |. chompWhile (\c -> Char.isDigit c)
        , succeed Nothing
        ]


{-| converts a string to a tuple made of command and timestamp

    import Product exposing (Product)
    import DataStructure exposing (Command(..), PayMethod(..))

    convertStringToCommand "add-1:1@19700101235959"
    --> (Add (Product 1 10 False, Just 1), Just 19700101235959)

    convertStringToCommand "add-1@19700101235959"
    --> (Add (Product 1 10 False, Nothing), Just 19700101235959)

    convertStringToCommand "pay-cash@19700101235959"
    --> (Pay Cash, Just 19700101235959)

    convertStringToCommand "pay-check@19700101235959"
    --> (Pay Check, Just 19700101235959)

    convertStringToCommand "end@19700101235959"
    --> (End, Just 19700101235959)

-}
convertStringToCommand : String -> ( Command, Maybe Int )
convertStringToCommand commandString =
    case run parseCommandStructure commandString of
        Err err ->
            ( Unknown, Nothing )

        Ok match ->
            let
                product =
                    match.secondArgument
                        |> Maybe.withDefault "0"
                        |> String.toInt
                        |> Maybe.andThen getProductById

                paymentMethod =
                    match.secondArgument |> Maybe.andThen getPayMethodFromString
            in
            case ( match.cmd, product, paymentMethod ) of
                ( "add", Just loadedProduct, Nothing ) ->
                    ( Add ( loadedProduct, match.quantity ), Just match.timestamp )

                ( "remove", Just loadedProduct, Nothing ) ->
                    ( Remove ( loadedProduct, match.quantity ), Just match.timestamp )

                ( "pay", Nothing, Just method ) ->
                    ( Pay method, Just match.timestamp )

                ( "end", Nothing, Nothing ) ->
                    ( End, Just match.timestamp )

                _ ->
                    ( Unknown, Nothing )
