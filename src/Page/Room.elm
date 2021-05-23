module Page.Room exposing (..)

import Browser.Navigation as Navigation
import Html exposing (Html, text)
import Url


type Msg
    = Msg


type Model
    = Model


init : Model
init =
    Model


view : Model -> Html Msg
view model =
    text "Welcome to Room 101!"
