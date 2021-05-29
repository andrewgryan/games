{-
   Main program, note only comments can exist before the
   module statement
-}


module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as Encode exposing (Value)
import LeaderBoard exposing (LeaderBoard)
import Page.Index as Index
import Ports exposing (messageReceiver)
import Route exposing (Route(..))
import Score exposing (Score)
import Set exposing (Set)
import Url



-- MODEL


type alias Model =
    { key : Key
    , page : Page
    , socket : Maybe String
    , sockets : Set String
    }


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
    in
    case route of
        Route.Index ->
            let
                page =
                    IndexPage (Index.init key)
            in
            ( { key = key
              , page = page
              , socket = Nothing
              , sockets = Set.empty
              }
            , Cmd.none
            )


decoderFlags : D.Decoder Flags
decoderFlags =
    D.map Flags
        (D.maybe (D.field "route" D.string))



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        -- NAVIGATION
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key
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
                            IndexPage (Index.init model.key)
                    in
                    ( { model | page = nextPage }, Cmd.none )

        -- PORT
        ( Recv value, IndexPage subModel ) ->
            case D.decodeValue portDecoder value of
                Ok portMsg ->
                    case portMsg of
                        LeaderBoardMsg leaderBoard ->
                            let
                                ( indexModel, cmd ) =
                                    Index.update (Index.gotLeaderBoard leaderBoard) subModel

                                page =
                                    IndexPage indexModel
                            in
                            ( { model | page = page }, Cmd.map IndexMsg cmd )

                        EnterMsg str ->
                            let
                                cmd =
                                    Encode.object
                                        [ ( "channel", Encode.string "public" )
                                        , ( "type", Encode.string "join" )
                                        , ( "payload"
                                          , Encode.object
                                                [ ( "id", Encode.string str )
                                                ]
                                          )
                                        ]
                                        |> Encode.encode 0
                                        |> Ports.sendMessage
                            in
                            ( { model | socket = Just str }, cmd )

                        JoinMsg str ->
                            ( { model
                                | sockets = Set.insert str model.sockets
                              }
                            , Cmd.none
                            )

                        ExitMsg str ->
                            ( { model
                                | sockets = Set.remove str model.sockets
                              }
                            , Cmd.none
                            )

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
            ( { model | page = nextPage }, Cmd.map IndexMsg nextCmd )



-- PORT DECODER


type PortMsg
    = LeaderBoardMsg LeaderBoard
    | ExitMsg String
    | EnterMsg String
    | JoinMsg String


type Channel
    = Enter
    | Exit
    | Private
    | Public


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
viewBody model =
    case model.page of
        IndexPage subModel ->
            Html.map IndexMsg (Index.view subModel)



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
