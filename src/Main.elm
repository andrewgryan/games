{-
   Main program, note only comments can exist before the
   module statement
-}


module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Json.Decode as D
import Json.Encode exposing (Value)
import LeaderBoard exposing (LeaderBoard)
import Page.Index as Index
import Ports exposing (messageReceiver)
import Route exposing (Route(..))
import Score exposing (Score)
import Url



-- SESSION


type Session
    = Session Key


toKey : Session -> Key
toKey (Session key) =
    key


fromKey : Key -> Session
fromKey key =
    Session key



-- MODEL


type Model
    = Model Session Page


type Page
    = IndexPage Index.Model



-- MSG


type Msg
    = IndexMsg Index.Msg
      -- PORT
    | Recv Value
      -- NAVIGATION
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url



-- INIT


type alias Flags =
    { route : Maybe String }


init : D.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init value url key =
    let
        flags =
            D.decodeValue decoderFlags value

        route =
            Route.fromUrl url

        session =
            fromKey key
    in
    case route of
        Route.Index ->
            let
                page =
                    IndexPage (Index.init key)
            in
            ( Model session page, Cmd.none )


decoderFlags : D.Decoder Flags
decoderFlags =
    D.map Flags
        (D.maybe (D.field "route" D.string))



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model session page) =
    let
        model =
            Model session page

        key =
            toKey session
    in
    case ( msg, page ) of
        -- NAVIGATION
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl key
                        (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        ( UrlChanged url, _ ) ->
            let
                route =
                    Route.fromUrl url
            in
            case route of
                Index ->
                    let
                        nextPage =
                            IndexPage (Index.init key)
                    in
                    ( Model session nextPage, Cmd.none )

        -- PORT
        ( Recv value, IndexPage subModel ) ->
            case D.decodeValue portDecoder value of
                Ok portMsg ->
                    case portMsg of
                        LeaderBoardMsg leaderBoard ->
                            let
                                ( indexModel, cmd ) =
                                    Index.update (Index.gotLeaderBoard leaderBoard) subModel
                            in
                            ( Model session (IndexPage indexModel), Cmd.map IndexMsg cmd )

                        EnterMsg str ->
                            let
                                _ =
                                    Debug.log "EnterMsg" str
                            in
                            ( model, Cmd.none )

                        JoinMsg str ->
                            let
                                _ =
                                    Debug.log "JoinMsg" str
                            in
                            ( model, Cmd.none )

                        ExitMsg str ->
                            let
                                _ =
                                    Debug.log "ExitMsg" str
                            in
                            ( model, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "Elm error" error
                    in
                    ( model, Cmd.none )

        -- PAGE
        ( IndexMsg subMsg, IndexPage subModel ) ->
            let
                ( nextModel, nextCmd ) =
                    Index.update subMsg subModel

                nextPage =
                    IndexPage nextModel
            in
            ( Model session nextPage, Cmd.map IndexMsg nextCmd )



-- PORT DECODER


type PortMsg
    = LeaderBoardMsg LeaderBoard
    | ExitMsg String
    | EnterMsg String
    | JoinMsg String


portDecoder : D.Decoder PortMsg
portDecoder =
    D.oneOf
        [ D.map LeaderBoardMsg LeaderBoard.decoder
        , D.field "type" D.string |> D.andThen payloadDecoder
        ]


payloadDecoder : String -> D.Decoder PortMsg
payloadDecoder label =
    case label of
        "enter" ->
            D.map EnterMsg (D.field "payload" (D.field "id" D.string))

        "join" ->
            D.map JoinMsg (D.field "payload" (D.field "id" D.string))

        "exit" ->
            D.map ExitMsg (D.field "payload" (D.field "id" D.string))

        _ ->
            D.fail "Unrecognised msg type"



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { body = [ viewBody model ]
    , title = "The Quiet Ryan's"
    }


viewBody : Model -> Html Msg
viewBody (Model key page) =
    case page of
        IndexPage model ->
            Html.map IndexMsg (Index.view model)



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv
