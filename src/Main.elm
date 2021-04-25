{-
   Main program, note only comments can exist before the
   module statement
-}


port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, li, text, ul)
import Html.Attributes exposing (attribute, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D


type alias Model =
    { draft : String
    , messages : List String
    }


type Msg
    = Send
    | DraftChanged String
    | Recv String



-- INIT


init : () -> ( Model, Cmd Msg )
init flags =
    ( { draft = "", messages = [] }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Send ->
            ( { model | draft = "" }, sendMessage model.draft )

        DraftChanged str ->
            ( { model | draft = str }, Cmd.none )

        Recv message ->
            ( { model | messages = model.messages ++ [ message ] }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Echo Chat" ]
        , ul []
            (List.map (\msg -> li [] [ text msg ]) model.messages)
        , input
            [ type_ "text"
            , placeholder "Draft"
            , onInput DraftChanged
            , on "keydown" (ifIsEnter Send)
            , value model.draft
            ]
            []
        , button [ onClick Send ] [ text "Send" ]
        ]



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



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv
