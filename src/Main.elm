{-
   Main program, note only comments can exist before the
   module statement
-}


module Main exposing (main, portDecoder)

import Browser
import Browser.Navigation exposing (Key)
import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , input
        , label
        , li
        , span
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , disabled
        , placeholder
        , type_
        , value
        )
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Json.Encode exposing (Value)
import LeaderBoard exposing (LeaderBoard)
import Page.Index as Index
import Page.New as New
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
    | NewPage New.Model



-- MSG


type Msg
    = IndexMsg Index.Msg
    | NewMsg New.Msg
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

        _ =
            Debug.log "route" route
    in
    case route of
        Route.Index ->
            let
                page =
                    IndexPage (Index.init key url)
            in
            ( Model session page, Cmd.none )

        Route.New ->
            let
                page =
                    NewPage New.init
            in
            ( Model session page, Cmd.none )

        Route.Quiz ->
            -- TODO support this route
            let
                page =
                    NewPage New.init
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
            ( model, Cmd.none )

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
                ( indexModel, cmd ) =
                    Index.update subMsg subModel
            in
            ( Model session (IndexPage indexModel), Cmd.map IndexMsg cmd )

        ( NewMsg subMsg, NewPage subModel ) ->
            let
                ( newModel, cmd ) =
                    New.update subMsg subModel
            in
            ( Model session (NewPage newModel), Cmd.map NewMsg cmd )

        ( _, _ ) ->
            ( model, Cmd.none )



-- PORT DECODER


portDecoder : D.Decoder LeaderBoard
portDecoder =
    LeaderBoard.decoder



-- D.oneOf
--     [ D.field "data" D.string |> D.andThen (\s -> D.succeed (Ack s))
--     , D.field "error" D.string |> D.andThen (\s -> D.succeed (Nack s))
--     ]
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

        NewPage model ->
            Html.map NewMsg (New.view model)



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
