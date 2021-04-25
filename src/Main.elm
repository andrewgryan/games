{-
   Main program, note only comments can exist before the
   module statement
-}


module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)


type alias Model =
    Int


type Msg
    = Increment
    | Decrement



-- INIT


init : Model
init =
    0



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Count: " ++ String.fromInt model) ]
        , button
            [ onClick Decrement
            , attribute "type" "button"
            ]
            [ text "-" ]
        , button
            [ onClick Increment
            , attribute "type" "button"
            ]
            [ text "+" ]
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
