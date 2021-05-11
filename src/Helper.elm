module Helper exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (classList)
import Json.Decode as D



-- CLASSES


classes : List String -> Attribute msg
classes strs =
    classList (List.map (\c -> ( c, True )) strs)



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )
