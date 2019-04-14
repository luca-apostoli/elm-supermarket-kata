module VerifyExamples.ParseCommand.ConvertStringToCommand0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import ParseCommand exposing (..)
import DataStructure exposing (Command(..))
import Product exposing (getProductById)







spec0 : Test.Test
spec0 =
    Test.test "#convertStringToCommand: \n\n    convertStringToCommand \"add-1@19700101235959\"\n    --> (Add (getProductById 1, Nothing), Just 19700101235959)" <|
        \() ->
            Expect.equal
                (
                convertStringToCommand "add-1@19700101235959"
                )
                (
                (Add (getProductById 1, Nothing), Just 19700101235959)
                )