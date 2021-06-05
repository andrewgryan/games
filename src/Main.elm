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
import Json.Decode as D
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



-- QUIZ


type Game
    = WaitingToPlay
    | Playing
    | ViewingResults



-- INIT


init : flags -> Url -> Key -> ( Model, Cmd msg )
init _ url key =
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
            , game = WaitingToPlay
            , leaderBoard =
                LeaderBoard.empty
            , quiz = Quiz.second
            , player = Player.thinking 0

            -- INTER-APP COMMS
            , socket = Nothing
            , sockets = Dict.empty
            , users = Dict.empty
            , quizzes = Dict.empty
            }
    in
    ( model, Cmd.none )



-- MODEL


type alias Model =
    { user : User
    , userDraft : String
    , errorMessage : Maybe D.Error
    , quiz : Quiz
    , game : Game
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
    , quizzes : Dict String Quiz
    }



-- MSG


type Msg
    = -- USER
      UserDraftChanged String
      -- QUIZ
    | StartQuiz
    | NextQuestion
    | PreviousQuestion
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
    | UserMsg Channel String String
    | PlayerMsg Channel Player String


type Channel
    = Public
    | Private



-- UPDATE


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
        StartQuiz ->
            let
                user =
                    User.loggedIn model.userDraft

                cmd =
                    Encode.object
                        [ ( "channel", Encode.string "public" )
                        , ( "type", Encode.string "user" )
                        , ( "payload"
                          , Encode.object
                                [ ( "id", encodeSocket model.socket )
                                , ( "user", Encode.string model.userDraft )
                                ]
                          )
                        ]
                        |> Ports.sendMessage
            in
            ( { model
                | game = Playing
                , user = user
              }
            , cmd
            )

        FinishQuiz ->
            let
                score =
                    Score.fromInt model.user (Quiz.tally model.quiz)

                cmd =
                    Outgoing.save score
                        |> Outgoing.encode
                        |> sendMessage
            in
            ( { model
                | game = ViewingResults
                , leaderBoard = LeaderBoard.empty
              }
            , cmd
            )

        NextQuestion ->
            let
                cmd =
                    Outgoing.answer model.user
                        |> Outgoing.encode
                        |> sendMessage
            in
            ( { model | quiz = Quiz.nextQuestion model.quiz }, cmd )

        PreviousQuestion ->
            ( { model | quiz = Quiz.previousQuestion model.quiz }, Cmd.none )

        SelectAnswer answer ->
            ( { model | quiz = Quiz.selectAnswer answer model.quiz }, Cmd.none )

        LockAnswerIn ->
            let
                newPlayer =
                    Player.done
                        (model.quiz
                            |> Quiz.getQuestionIndex
                        )

                cmd =
                    Encode.object
                        [ ( "channel", Encode.string "public" )
                        , ( "type", Encode.string "player" )
                        , ( "payload"
                          , Encode.object
                                [ ( "id", encodeSocket model.socket )
                                , ( "player", Player.encode newPlayer )
                                ]
                          )
                        ]
                        |> Ports.sendMessage
            in
            case Player.chooseMove newPlayer (Dict.values model.sockets) of
                Forward ->
                    let
                        quiz =
                            Quiz.nextQuestion model.quiz

                        questionIndex =
                            Quiz.getQuestionIndex quiz
                    in
                    ( { model
                        | quiz = quiz
                        , player = Thinking questionIndex
                      }
                    , cmd
                    )

                Wait ->
                    -- WAIT FOR OTHER PLAYERS
                    ( { model
                        | player = newPlayer
                      }
                    , cmd
                    )

        -- PORT
        Recv value ->
            case D.decodeValue portDecoder value of
                Ok portMsg ->
                    case portMsg of
                        -- LEADERBOARD
                        LeaderBoardMsg leaderBoard ->
                            ( { model | leaderBoard = leaderBoard }, Cmd.none )

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
                                        |> Ports.sendMessage
                            in
                            ( { model | socket = Just str }, cmd )

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

                        UserMsg channel str id ->
                            let
                                cmd =
                                    case channel of
                                        Public ->
                                            case model.user of
                                                Anonymous ->
                                                    Cmd.none

                                                LoggedIn name ->
                                                    Encode.object
                                                        [ ( "channel", Encode.string "private" )
                                                        , ( "to", Encode.string id )
                                                        , ( "type", Encode.string "user" )
                                                        , ( "payload"
                                                          , Encode.object
                                                                [ ( "id", encodeSocket model.socket )
                                                                , ( "user", Encode.string name )
                                                                ]
                                                          )
                                                        ]
                                                        |> Ports.sendMessage

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
                                            Encode.object
                                                [ ( "channel", Encode.string "private" )
                                                , ( "to", Encode.string socketID )
                                                , ( "type", Encode.string "player" )
                                                , ( "payload"
                                                  , Encode.object
                                                        [ ( "id", encodeSocket model.socket )
                                                        , ( "player", Player.encode model.player )
                                                        ]
                                                  )
                                                ]
                                                |> Ports.sendMessage

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
                                    in
                                    ( { model
                                        | sockets = sockets
                                        , quiz = quiz
                                        , player = Thinking questionIndex
                                      }
                                    , cmd
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
            { body = [ viewBody model ]
            , title = "The Quiet Ryan's"
            }

        Admin ->
            { body = [ viewAdmin model.adminText ]
            , title = "Admin"
            }


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


viewBody : Model -> Html Msg
viewBody model =
    let
        friends =
            Dict.values model.users |> List.sort
    in
    case model.game of
        WaitingToPlay ->
            viewStartPage model.userDraft friends

        Playing ->
            case model.player of
                Done _ ->
                    viewWaitingForFriends

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

        ViewingResults ->
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


viewStartPage : String -> List String -> Html Msg
viewStartPage draftName friends =
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
                    , on "keydown" (ifIsEnter StartQuiz)
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
                    [ onClick StartQuiz
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


previousButton : Html Msg
previousButton =
    button
        [ class <|
            String.join " " <|
                [ "bg-white"
                , "border"
                , "border-blue-500"
                , "text-near-black"
                , "flex-grow"
                ]
        , onClick PreviousQuestion
        ]
        [ text "Go back" ]


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


viewWaitingForFriends : Html Msg
viewWaitingForFriends =
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
                [ text "Waiting for everyone..." ]
            ]
        ]


nextButton : Bool -> Html Msg
nextButton isDisabled =
    button
        [ primaryButtonStyle
        , class "bg-green-500"
        , class "flex-grow"
        , onClick NextQuestion
        , disabled isDisabled
        ]
        [ div
            [ class <|
                String.join " " <|
                    [ "flex"
                    , "flex-row"
                    , "justify-center"
                    , "items-center"
                    , "uppercase"
                    ]
            ]
            [ div [ class "px-2" ] [ text "To next question" ]
            , Heroicons.arrowRight
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
