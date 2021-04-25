{-
   Main program, note only comments can exist before the
   module statement
-}


module Main exposing (main)

import Browser
import Html exposing (Html, text)


type alias Model =
    Int


type Msg
    = NoOp



-- INIT


init : Model
init =
    0



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    text "Hello, World!"


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
