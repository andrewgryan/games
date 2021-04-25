{-
   Main program, note only comments can exist before the
   module statement
-}


port module Main exposing (main, portDecoder)

import Browser
import Html exposing (Html, button, div, h1, input, li, text, ul)
import Html.Attributes exposing (attribute, placeholder, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Json.Encode exposing (Value)



-- MODEL


type alias Model =
    { user : User
    , userDraft : String
    , draft : String
    , messages : List String
    , status : Status
    , errorMessage : Maybe D.Error
    }


type User
    = Anonymous
    | LoggedIn String



-- MSG


type Msg
    = Send
    | DraftChanged String
    | Recv Value
    | WebSocket Status
    | UserSend
    | UserDraftChanged String


type Status
    = NotStarted
    | Opened
    | Closed



-- INIT


init : () -> ( Model, Cmd Msg )
init flags =
    ( { draft = ""
      , messages = []
      , status = NotStarted
      , errorMessage = Nothing
      , user = Anonymous
      , userDraft = ""
      }
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

        Recv value ->
            case D.decodeValue portDecoder value of
                Ok portMessage ->
                    case portMessage of
                        Ack message ->
                            ( { model | messages = model.messages ++ [ message ] }, Cmd.none )

                        Nack _ ->
                            ( { model | status = Closed }, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just error }, Cmd.none )

        WebSocket status ->
            ( { model | status = status }, Cmd.none )

        -- USER
        UserSend ->
            ( { model | userDraft = "", user = LoggedIn model.userDraft }, Cmd.none )

        UserDraftChanged str ->
            ( { model | userDraft = str }, Cmd.none )



-- PORT DECODER


type PortMessage
    = Ack String
    | Nack String


portDecoder : D.Decoder PortMessage
portDecoder =
    D.oneOf
        [ D.field "data" D.string |> D.andThen (\s -> D.succeed (Ack s))
        , D.field "error" D.string |> D.andThen (\s -> D.succeed (Nack s))
        ]



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
        , viewStatus model.status

        -- USER Login
        , viewUser model.user model.userDraft
        ]


viewUser : User -> String -> Html Msg
viewUser user draftName =
    case user of
        Anonymous ->
            div []
                [ div [] [ text "Not signed in" ]
                , input
                    [ type_ "text"
                    , placeholder "Draft"
                    , onInput UserDraftChanged
                    , on "keydown" (ifIsEnter UserSend)
                    , value draftName
                    ]
                    []
                , button [ onClick UserSend ] [ text "Send" ]
                ]

        LoggedIn str ->
            div [] [ text ("Signed in as: " ++ str) ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Closed ->
            div [] [ text "Connection lost" ]

        _ ->
            div [] []



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


port messageReceiver : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv
