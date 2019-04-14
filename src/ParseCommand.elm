module ParseCommand exposing (convertStringToCommand)

import DataStructure exposing (Command(..), PayMethod(..), getPayMethodFromString)
import Parser exposing ((|.), (|=), Parser, chompWhile, end, getChompedString, int, keyword, map, oneOf, run, succeed, symbol)
import Product exposing (getProductById)


type alias CommandStructure =
    { cmd : String
    , secondArgument : String
    , quantity : Maybe Int
    , timestamp : Int
    }


parseCommandStructure : Parser CommandStructure
parseCommandStructure =
    oneOf
        [ succeed CommandStructure
            |= parseCommand
            |. symbol "-"
            |= parseSecondArgument
            |. symbol ":"
            |= map Just int
            |. symbol "@"
            |= int
            |. end
        , succeed CommandStructure
            |= parseCommand
            |. symbol "-"
            |= parseSecondArgument
            |= Parser.succeed Nothing
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
        , map (\_ -> "end") (keyword "end")
        ]


parseSecondArgument : Parser String
parseSecondArgument =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> Char.isAlphaNum c && c /= '@' && c /= ':')


{-| converts a string to a tuple made of command and timestamp

    import Product exposing (getProductById)
    import DataStructure exposing (Command(..))

    convertStringToCommand "add-1:1@19700101235959"
    --> (Add (getProductById 1, Just 1), Just 19700101235959)

    convertStringToCommand "add-1@19700101235959"
    --> (Add (getProductById 1, Nothing), Just 19700101235959)

-}
convertStringToCommand : String -> ( Command, Maybe Int )
convertStringToCommand commandString =
    case run parseCommandStructure commandString of
        Err err ->
            let
                _ =
                    Debug.toString err
                        |> Debug.log
            in
            ( End, Nothing )

        Ok match ->
            let
                product =
                    String.toInt match.secondArgument |> Maybe.andThen getProductById
            in
            case match.cmd of
                "add" ->
                    ( Add ( product, match.quantity ), Just match.timestamp )

                "remove" ->
                    ( Remove ( product, match.quantity ), Just match.timestamp )

                "pay" ->
                    ( Pay (getPayMethodFromString match.secondArgument), Just match.timestamp )

                _ ->
                    ( End, Just match.timestamp )
