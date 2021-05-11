module Page.New exposing (Model, init, view)

import Html exposing (Html, text)


type Model
    = Draft String


init : Model
init =
    Draft "Hello"


view : Html msg
view =
    text "New Quiz!"
