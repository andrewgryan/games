module Main exposing (main)

import Browser
import Browser.Navigation as Navigation exposing (Key)
import Container
import Dict exposing (Dict)
import Header
import Helper exposing (classes, ifIsEnter)
import Heroicons exposing (menu, pencil)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as Encode
import LeaderBoard exposing (LeaderBoard)
import Outgoing
import Player exposing (Move(..), Player(..))
import Ports exposing (messageReceiver, sendMessage)
import Quiz exposing (Answer, Question, Quiz)
import Review
import Route exposing (Route(..))
import Score exposing (Score)
import Set exposing (Set)
import Socket.ID exposing (ID)
import Url exposing (Url)
import User exposing (User(..))



-- INIT


type alias Flags =
    { user : Maybe User
    , quiz : Maybe Quiz
    , player : Maybe Player
    , leaderBoard : Maybe LeaderBoard
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    D.map4 Flags
        (D.field "user" (D.maybe User.decoder))
        (D.field "quiz" (D.maybe Quiz.decoder))
        (D.field "player" (D.maybe Player.decoder))
        (D.field "leaderBoard" (D.maybe LeaderBoard.decoder))


init : D.Value -> Url -> Key -> ( Model, Cmd msg )
init value url key =
    let
        model =
            { key = key
            , errorMessage = Nothing
            , user = User.anonymous
            , userDraft = ""

            -- ADMIN
            , adminText = ""

            -- ROUTING
            , route = Route.fromUrl url

            -- QUIZ
            , leaderBoard =
                LeaderBoard.empty
            , quiz = Quiz.second
            , player = Player.thinking 0

            -- INTER-APP COMMS
            , socket = Nothing
            , sockets = Dict.empty
            , users = Dict.empty
            }
    in
    case D.decodeValue flagsDecoder value of
        Ok flags ->
            ( { model
                | user = Maybe.withDefault model.user flags.user
                , quiz = Maybe.withDefault model.quiz flags.quiz
                , player = Maybe.withDefault model.player flags.player
                , leaderBoard = Maybe.withDefault model.leaderBoard flags.leaderBoard
              }
            , Cmd.none
            )

        Err _ ->
            ( model, Cmd.none )



-- MODEL


type alias Model =
    { user : User
    , userDraft : String
    , errorMessage : Maybe D.Error
    , quiz : Quiz
    , player : Player
    , leaderBoard : LeaderBoard

    -- ADMIN
    , adminText : String

    -- ROUTING
    , route : Route

    -- Navigation
    , key : Navigation.Key

    -- Inter-app communication
    , socket : Maybe String
    , sockets : Dict String Player
    , users : Dict String String
    }



-- MSG


type Msg
    = -- USER
      UserDraftChanged String
      -- QUIZ
    | GotUsername
    | StartQuiz
    | FinishQuiz
    | SelectAnswer Answer
    | LockAnswerIn
      -- PORT
    | Recv Encode.Value
      -- NAVIGATION
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
      -- ADMIN
    | ClearScoreboard
    | GotText (Result Http.Error String)


type PortMsg
    = LeaderBoardMsg LeaderBoard
    | ExitMsg String
    | EnterMsg String
    | JoinMsg Channel Socket.ID.ID
    | StartMsg Channel Socket.ID.ID
    | UserMsg Channel String String
    | PlayerMsg Channel Player String


type Channel
    = Public
    | Private


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
            D.map2 JoinMsg
                (D.field "channel" channelDecoder)
                (D.field "payload" (D.map Socket.ID.ID (D.field "id" D.string)))

        "start" ->
            D.map2 StartMsg
                (D.field "channel" channelDecoder)
                (D.field "payload" (D.map Socket.ID.ID (D.field "id" D.string)))

        "user" ->
            D.map3 UserMsg
                (D.field "channel" channelDecoder)
                (D.field "payload" (D.field "user" D.string))
                (D.field "payload" (D.field "id" D.string))

        "player" ->
            D.map3 PlayerMsg
                (D.field "channel" channelDecoder)
                (D.field "payload" (D.field "player" Player.decoder))
                (D.field "payload" (D.field "id" D.string))

        "exit" ->
            D.map ExitMsg (D.field "payload" (D.field "id" D.string))

        _ ->
            D.fail "Unrecognised msg type"


channelDecoder : D.Decoder Channel
channelDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "public" ->
                        D.succeed Public

                    "private" ->
                        D.succeed Private

                    _ ->
                        D.fail "Unknown channel type"
            )


joinRoomPayload : Int -> Encode.Value
joinRoomPayload n =
    Encode.object
        [ ( "room", Encode.int n )
        ]


encodeSocket : Maybe String -> Encode.Value
encodeSocket maybeSocket =
    case maybeSocket of
        Just str ->
            Encode.string str

        Nothing ->
            Encode.null


sessionStorage : String -> String -> Cmd Msg
sessionStorage key value =
    Encode.object
        [ ( "type", Encode.string "sessionStorage" )
        , ( "payload"
          , Encode.object
                [ ( "key", Encode.string key )
                , ( "value", Encode.string value )
                ]
          )
        ]
        |> Ports.sendMessage



-- BROADCAST


type alias Socket =
    Maybe String


broadcastPublicJoin : String -> Cmd Msg
broadcastPublicJoin str =
    Encode.object
        [ ( "channel", Encode.string "public" )
        , ( "type", Encode.string "join" )
        , ( "payload"
          , Encode.object
                [ ( "id", Encode.string str )
                ]
          )
        ]
        |> Ports.sendMessage


broadcastPublicUser : Socket -> String -> Cmd Msg
broadcastPublicUser socketId userName =
    Encode.object
        [ ( "channel", Encode.string "public" )
        , ( "type", Encode.string "user" )
        , ( "payload"
          , Encode.object
                [ ( "id", encodeSocket socketId )
                , ( "user", Encode.string userName )
                ]
          )
        ]
        |> Ports.sendMessage


broadcastPublicPlayer : Socket -> Player -> Cmd Msg
broadcastPublicPlayer socketId player =
    Encode.object
        [ ( "channel", Encode.string "public" )
        , ( "type", Encode.string "player" )
        , ( "payload"
          , Encode.object
                [ ( "id", encodeSocket socketId )
                , ( "player", Player.encode player )
                ]
          )
        ]
        |> Ports.sendMessage


broadcastPrivatePlayer : String -> Socket -> Player -> Cmd Msg
broadcastPrivatePlayer toSocketID socket player =
    Encode.object
        [ ( "channel", Encode.string "private" )
        , ( "to", Encode.string toSocketID )
        , ( "type", Encode.string "player" )
        , ( "payload"
          , Encode.object
                [ ( "id", encodeSocket socket )
                , ( "player", Player.encode player )
                ]
          )
        ]
        |> Ports.sendMessage


broadcastPrivateUser : String -> Socket -> String -> Cmd Msg
broadcastPrivateUser toSocketID socket userName =
    Encode.object
        [ ( "channel", Encode.string "private" )
        , ( "to", Encode.string toSocketID )
        , ( "type", Encode.string "user" )
        , ( "payload"
          , Encode.object
                [ ( "id", encodeSocket socket )
                , ( "user", Encode.string userName )
                ]
          )
        ]
        |> Ports.sendMessage



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- ADMIN
        ClearScoreboard ->
            let
                cmd =
                    Http.get
                        { url = "/clear"
                        , expect = Http.expectString GotText
                        }
            in
            ( model, cmd )

        GotText result ->
            case result of
                Ok str ->
                    ( { model | adminText = str }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        -- NAVIGATION
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key
                        (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url
            in
            ( { model | route = route }, Cmd.none )

        UserDraftChanged str ->
            ( { model | userDraft = str }, Cmd.none )

        -- QUIZ
        GotUsername ->
            let
                user =
                    User.loggedIn model.userDraft

                cmd =
                    Cmd.batch
                        [ -- BROADCAST
                          broadcastPublicUser model.socket model.userDraft

                        -- SESSION STORAGE
                        , sessionStorage "user" (User.toString user)

                        -- GO TO WAITING ROOM
                        , Navigation.pushUrl model.key "/waiting"
                        ]
            in
            ( { model
                | user = user
              }
            , cmd
            )

        StartQuiz ->
            let
                cmd =
                    Cmd.batch
                        [ -- GO TO QUIZ
                          Navigation.pushUrl model.key "/quiz"

                        -- BROADCAST TO PLAYERS
                        , Encode.object
                            [ ( "channel", Encode.string "public" )
                            , ( "type", Encode.string "start" )
                            , ( "payload"
                              , Encode.object
                                    [ ( "id", encodeSocket model.socket )
                                    ]
                              )
                            ]
                            |> Ports.sendMessage
                        ]
            in
            ( model, cmd )

        FinishQuiz ->
            let
                score =
                    Score.fromInt model.user (Quiz.tally model.quiz)

                cmd =
                    Cmd.batch
                        [ Outgoing.save score
                            |> Outgoing.encode
                            |> sendMessage

                        -- GO TO SCOREBOARD
                        , Navigation.pushUrl model.key "/scoreboard"
                        ]
            in
            ( { model
                | leaderBoard = LeaderBoard.empty
              }
            , cmd
            )

        SelectAnswer answer ->
            let
                quiz =
                    Quiz.selectAnswer answer model.quiz

                cmd =
                    sessionStorage "quiz" (Quiz.toString quiz)
            in
            ( { model | quiz = quiz }, cmd )

        LockAnswerIn ->
            let
                newPlayer =
                    Player.done
                        (model.quiz
                            |> Quiz.getQuestionIndex
                        )

                cmd =
                    broadcastPublicPlayer model.socket newPlayer
            in
            case Player.chooseMove newPlayer (Dict.values model.sockets) of
                Forward ->
                    let
                        quiz =
                            Quiz.nextQuestion model.quiz

                        questionIndex =
                            Quiz.getQuestionIndex quiz

                        player =
                            Thinking questionIndex

                        cmds =
                            Cmd.batch
                                [ cmd
                                , sessionStorage "quiz" (Quiz.toString quiz)
                                , sessionStorage "player" (Player.toString player)
                                ]
                    in
                    ( { model
                        | quiz = quiz
                        , player = player
                      }
                    , cmds
                    )

                Wait ->
                    -- WAIT FOR OTHER PLAYERS
                    let
                        cmds =
                            Cmd.batch
                                [ cmd
                                , sessionStorage "player" (Player.toString newPlayer)
                                ]
                    in
                    ( { model
                        | player = newPlayer
                      }
                    , cmds
                    )

        -- PORT
        Recv value ->
            case D.decodeValue portDecoder value of
                Ok portMsg ->
                    case portMsg of
                        -- LEADERBOARD
                        LeaderBoardMsg leaderBoard ->
                            let
                                cmd =
                                    sessionStorage "leaderBoard" (LeaderBoard.toString leaderBoard)
                            in
                            ( { model | leaderBoard = leaderBoard }, cmd )

                        EnterMsg str ->
                            let
                                cmd =
                                    broadcastPublicJoin str

                                socket =
                                    Just str
                            in
                            case model.user of
                                Anonymous ->
                                    ( { model | socket = socket }, cmd )

                                LoggedIn userName ->
                                    ( { model | socket = socket }
                                    , Cmd.batch
                                        [ cmd
                                        , broadcastPublicUser socket userName
                                        ]
                                    )

                        JoinMsg channel socketID ->
                            let
                                str =
                                    Socket.ID.toString socketID
                            in
                            ( { model
                                | sockets = Dict.insert str (Player.thinking 0) model.sockets
                              }
                            , Cmd.none
                            )

                        StartMsg channel socketID ->
                            let
                                cmd =
                                    Cmd.batch
                                        [ -- GO TO QUIZ
                                          Navigation.pushUrl model.key "/quiz"
                                        ]
                            in
                            ( model, cmd )

                        UserMsg channel str id ->
                            let
                                cmd =
                                    case channel of
                                        Public ->
                                            case model.user of
                                                Anonymous ->
                                                    Cmd.none

                                                LoggedIn name ->
                                                    Cmd.batch
                                                        [ broadcastPrivateUser id
                                                            model.socket
                                                            name
                                                        , broadcastPrivatePlayer id
                                                            model.socket
                                                            model.player
                                                        ]

                                        Private ->
                                            Cmd.none
                            in
                            ( { model
                                | users = Dict.insert id str model.users
                              }
                            , cmd
                            )

                        PlayerMsg channel player socketID ->
                            let
                                cmd =
                                    case channel of
                                        Public ->
                                            broadcastPrivatePlayer socketID
                                                model.socket
                                                model.player

                                        Private ->
                                            Cmd.none

                                sockets =
                                    Dict.insert socketID player model.sockets
                            in
                            case Player.chooseMove model.player (Dict.values sockets) of
                                Forward ->
                                    -- ALL DONE
                                    let
                                        quiz =
                                            Quiz.nextQuestion model.quiz

                                        questionIndex =
                                            Quiz.getQuestionIndex quiz

                                        nextPlayer =
                                            Thinking questionIndex

                                        cmds =
                                            Cmd.batch
                                                [ cmd
                                                , sessionStorage "quiz" (Quiz.toString quiz)
                                                , sessionStorage "player" (Player.toString nextPlayer)
                                                ]
                                    in
                                    ( { model
                                        | sockets = sockets
                                        , quiz = quiz
                                        , player = nextPlayer
                                      }
                                    , cmds
                                    )

                                Wait ->
                                    -- WAIT FOR EVERYONE
                                    ( { model
                                        | sockets = sockets
                                      }
                                    , cmd
                                    )

                        ExitMsg str ->
                            ( { model
                                | sockets = Dict.remove str model.sockets
                                , users = Dict.remove str model.users
                              }
                            , Cmd.none
                            )

                Err error ->
                    -- TODO report errors
                    -- let
                    --     _ =
                    --         Debug.log "error" error
                    -- in
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Index ->
            { body = [ viewIndex model ]
            , title = "The Quiet Ryan's"
            }

        Quiz ->
            { body = [ viewBody model ]
            , title = "The Quiet Ryan's"
            }

        WaitingRoom ->
            { body = [ viewWaitingRoom model ]
            , title = "The Quiet Ryan's"
            }

        ScoreBoard ->
            { body = [ viewScoreBoard model ]
            , title = "The Quiet Ryan's"
            }

        Admin ->
            { body = [ viewAdmin model.adminText ]
            , title = "Admin"
            }


viewWaitingRoom : Model -> Html Msg
viewWaitingRoom model =
    let
        players =
            Dict.values model.users
    in
    case players of
        [] ->
            viewWaitingForFriends "Waiting for people to join..."

        _ ->
            div
                [ classes
                    [ "flex"
                    , "flex-col"
                    , "h-screen"
                    , "w-screen"
                    ]
                ]
                [ Header.view
                , div
                    [ classes
                        [ "flex"
                        , "flex-col"
                        , "items-center"
                        , "justify-between"
                        , "flex-grow"
                        ]
                    ]
                    [ viewUser model.user
                    , viewPlayers players
                    ]
                ]


viewPlayers : List String -> Html Msg
viewPlayers players =
    div
        [ classes
            [ "flex"
            , "flex-col"
            , "flex-grow"
            , "justify-center"
            , "items-center"
            , "space-y-2"
            , "w-full"
            ]
        ]
        [ h1
            [ classes
                [ "text-2xl"
                , "font-bold"
                , "text-teal-600"
                ]
            ]
            [ text "Other players" ]
        , div []
            (List.map
                (\u ->
                    div [] [ text u ]
                )
                players
            )
        , button
            [ classes
                [ "p-4"
                , "bg-teal-700"
                , "w-full"
                , "uppercase"
                , "font-bold"
                , "text-white"
                ]
            , onClick StartQuiz
            ]
            [ text "Everyone's here" ]
        ]


classes : List String -> Attribute Msg
classes =
    class << String.join " "


viewAdmin : String -> Html Msg
viewAdmin str =
    div
        [ class <|
            String.join " " <|
                [ "flex"
                , "flex-col"
                , "justify-center"
                , "items-center"
                , "h-screen"
                ]
        ]
        [ button
            [ class <|
                String.join " " <|
                    [ "border-red-600"
                    , "border-2"
                    , "text-red-600"
                    , "p-4"
                    , "mx-2"
                    ]
            , onClick ClearScoreboard
            ]
            [ text "Clear scoreboard" ]
        , div [] [ text str ]
        ]


viewIndex : Model -> Html Msg
viewIndex model =
    viewStartPage model.userDraft


viewBody : Model -> Html Msg
viewBody model =
    let
        friends =
            Dict.values model.users |> List.sort
    in
    case model.player of
        Done _ ->
            viewWaitingForFriends "Waiting for everyone..."

        Thinking _ ->
            let
                remaining =
                    Quiz.getNext model.quiz

                question =
                    Quiz.getQuestion model.quiz
            in
            div
                [ class "flex"
                , class "flex-col"
                , class "h-screen"
                ]
                [ -- QUIZ
                  Header.view
                , div
                    [ class "flex"
                    , class "flex-row"
                    , class "justify-between"
                    ]
                    [ div []
                        [ viewUser model.user
                        , viewFriends friends
                        ]
                    , viewRemaining remaining
                    ]
                , div
                    [ class "flex"
                    , class "flex-col"
                    , class "justify-center"
                    , class "flex-grow"
                    , class "space-y-4"
                    ]
                    [ div
                        [ class "flex"
                        , class "justify-center"
                        , class "items-center"
                        ]
                        [ Quiz.viewQuestion SelectAnswer question
                        ]
                    , viewNav model.quiz
                    ]
                ]


viewScoreBoard : Model -> Html Msg
viewScoreBoard model =
    div []
        [ Header.view
        , viewError model.errorMessage
        , LeaderBoard.view model.leaderBoard
        , Review.view model.quiz
        ]


viewRooms : (Int -> Msg) -> Html Msg
viewRooms toMsg =
    div
        [ classes
            [ "flex flex-col"
            , "h-screen"
            ]
        ]
        [ -- NAV
          Header.view

        -- CONTENT
        , div
            [ classes
                [ "flex-auto"
                , "p-4"
                , "flex"
                , "flex-col"
                , "space-y-4"
                ]
            ]
            [ div
                [ classes
                    [ "text-xl"
                    , "py-4"
                    ]
                ]
                [ text "Q. Odd one out?" ]

            -- ANSWERS
            , div
                [ classes
                    [ "bg-gray-100"
                    , "shadow"
                    , "p-4"
                    ]
                ]
                [ text "Giraffe" ]
            , div
                [ classes
                    [ "bg-gray-100"
                    , "shadow"
                    , "p-4"
                    ]
                ]
                [ text "Tiger" ]
            , div
                [ classes
                    [ "bg-gray-100"
                    , "shadow"
                    , "p-4"
                    ]
                ]
                [ text "Banana" ]
            , div
                [ classes
                    [ "bg-gray-100"
                    , "shadow"
                    , "p-4"
                    ]
                ]
                [ text "Dolphin" ]

            -- CONTROLS
            , div
                [ classes
                    [ "flex"
                    , "flex-row"
                    , "justify-end"
                    ]
                ]
                [ div
                    [ classes
                        [ "bg-teal-300"
                        , "p-4"
                        , "rounded-full"
                        , "shadow"
                        , "text-gray-700"
                        ]
                    ]
                    [ Heroicons.chevronRight
                    ]
                ]
            ]
        ]


viewFriends : List String -> Html Msg
viewFriends friends =
    div
        [ class "font-light text-sm p-1"
        ]
        [ div
            [ class "inline"
            ]
            [ text "Friends online: " ]
        , div
            [ class "font-bold"
            , class "inline"
            ]
            [ text (String.join " " friends) ]
        ]


viewStartPage : String -> Html Msg
viewStartPage draftName =
    div
        [ class "w-screen"
        , class "h-screen"
        , class "flex"
        , class "flex-col"
        ]
        [ Header.view
        , div
            [ class <|
                String.join " " <|
                    [ "flex"
                    , "flex-col"
                    , "justify-center"
                    , "items-center"
                    , "flex-grow"
                    , "px-2"
                    , "space-y-4"
                    ]
            ]
            [ div
                [ class "w-full"
                ]
                [ label
                    [ class <|
                        String.join " " <|
                            [ "block"
                            , "text-gray-700"
                            , "text-md"
                            , "font-bold"
                            ]
                    ]
                    [ text "Enter name:" ]
                , input
                    [ type_ "text"
                    , placeholder "Enter text"
                    , onInput UserDraftChanged
                    , on "keydown" (ifIsEnter GotUsername)
                    , value draftName
                    , class <|
                        String.join " " <|
                            [ "shadow"
                            , "appearence-none"
                            , "border"
                            , "bg-gray-200"
                            , "py-4"
                            , "px-4"
                            , "text-xl"
                            , "w-full"
                            ]
                    ]
                    []
                ]
            , if draftName /= "" then
                button
                    [ onClick GotUsername
                    , class <|
                        String.join " " <|
                            [ "w-full"
                            , "p-4"
                            , "bg-blue-500"
                            , "uppercase"
                            , "text-xl"
                            , "text-white"
                            ]
                    ]
                    [ text "Start Quiz" ]

              else
                text ""
            ]
        ]


primaryButtonStyle : Html.Attribute Msg
primaryButtonStyle =
    class <|
        String.join " " <|
            [ "text-white"
            , "p-4"
            , "py-6"
            ]


viewNav : Quiz -> Html Msg
viewNav quiz =
    let
        question =
            Quiz.getQuestion quiz

        remaining =
            Quiz.getNext quiz
    in
    div
        [ class "flex"
        , class "pb-8"
        ]
        [ if Quiz.answered question then
            case remaining of
                [] ->
                    finishButton FinishQuiz

                _ ->
                    lockInButton LockAnswerIn

          else
            text ""
        ]


viewRemaining : List Question -> Html Msg
viewRemaining remaining =
    let
        n =
            List.length remaining
    in
    div
        [ class "text-sm text-gray-400 px-2"
        ]
        [ text (String.fromInt n ++ " " ++ plural n ++ " to go")
        ]


plural : Int -> String
plural n =
    if n > 0 then
        "questions"

    else
        "question"


lockInButton : Msg -> Html Msg
lockInButton toMsg =
    button
        [ class <|
            String.join " " <|
                [ "bg-yellow-400"
                , "flex-grow"
                , "uppercase"
                , "px-4"
                , "py-6"
                , "mx-2"
                ]
        , onClick toMsg
        ]
        [ div
            [ class <|
                String.join " " <|
                    [ "flex"
                    , "flex-row"
                    , "justify-center"
                    , "items-center"
                    ]
            ]
            [ Heroicons.lockOpen
            , div [ class "pl-2" ] [ text "Lock answer in" ]
            ]
        ]


viewWaitingForFriends : String -> Html Msg
viewWaitingForFriends messageText =
    div
        [ class <|
            String.join " " <|
                [ "bg-purple-500"
                , "text-white"
                , "flex"
                , "flex-row"
                , "justify-center"
                , "items-center"
                , "h-screen"
                ]
        ]
        [ div
            [ class <|
                String.join " " <|
                    [ "flex"
                    , "flex-row"
                    , "justify-center"
                    , "items-center"
                    , "animate-pulse"
                    ]
            ]
            [ Heroicons.sparkle
            , div
                [ class <|
                    String.join " " <|
                        [ "pl-2"
                        , "uppercase"
                        ]
                ]
                [ text messageText ]
            ]
        ]


finishButton : Msg -> Html Msg
finishButton toMsg =
    button
        [ primaryButtonStyle
        , class <|
            String.join " " <|
                [ "flex-grow"
                , "bg-blue-600"
                , "uppercase"
                , "mx-2"
                ]
        , onClick toMsg
        ]
        [ text "Finish quiz" ]


viewError : Maybe D.Error -> Html Msg
viewError maybeError =
    case maybeError of
        Nothing ->
            text ""

        Just error ->
            div [] [ text (D.errorToString error) ]


viewUser : User -> Html Msg
viewUser user =
    div
        [ class "font-light text-sm p-1"
        ]
        [ text "Signed in as "
        , span
            [ class "font-bold"
            ]
            [ text (User.toString user) ]
        ]



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
