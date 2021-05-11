module Page.New exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Events exposing (onInput)



-- MSG


type Msg
    = DraftChanged String



-- MODEL


type Model
    = Draft String



--INIT


init : Model
init =
    Draft "Hello"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged str ->
            ( Draft str, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Draft str ->
            div []
                [ text str
                , input [ onInput DraftChanged ] []
                ]
