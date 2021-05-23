module Page.Room exposing (..)

import Browser.Navigation as Navigation
import Html exposing (Html, text)
import Url


type Msg
    = Msg


type alias Model =
    ID


type ID
    = ID Int


fromInt : Int -> ID
fromInt n =
    ID n


init : Int -> Model
init n =
    fromInt n


view : Model -> Html Msg
view (ID n) =
    text ("Welcome to Room " ++ String.fromInt n ++ "!")
