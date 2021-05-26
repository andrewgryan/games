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
import Page.Room as Room
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
    | RoomPage Room.Model



-- MSG


type Msg
    = IndexMsg Index.Msg
    | RoomMsg Room.Msg
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

        Route.Room n ->
            let
                page =
                    RoomPage (Room.init key n)
            in
            ( Model session page, Cmd.none )

        Route.Play n ->
            let
                page =
                    RoomPage (Room.init key n)
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

                Room n ->
                    let
                        nextPage =
                            RoomPage (Room.init key n)
                    in
                    ( Model session nextPage, Cmd.none )

                Play n ->
                    let
                        nextPage =
                            RoomPage (Room.init key n)
                    in
                    ( Model session nextPage, Cmd.none )

        -- PORT
        ( Recv value, IndexPage subModel ) ->
            case D.decodeValue portDecoder value of
                Ok leaderBoard ->
                    let
                        ( indexModel, cmd ) =
                            Index.update (Index.gotLeaderBoard leaderBoard) subModel
                    in
                    ( Model session (IndexPage indexModel), Cmd.map IndexMsg cmd )

                Err error ->
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

        ( RoomMsg subMsg, RoomPage subModel ) ->
            let
                ( nextModel, nextCmd ) =
                    Room.update subMsg subModel

                nextPage =
                    RoomPage nextModel
            in
            ( Model session nextPage, Cmd.map RoomMsg nextCmd )

        ( _, _ ) ->
            ( model, Cmd.none )



-- PORT DECODER


portDecoder : D.Decoder LeaderBoard
portDecoder =
    LeaderBoard.decoder



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

        RoomPage model ->
            Html.map RoomMsg (Room.view model)



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
