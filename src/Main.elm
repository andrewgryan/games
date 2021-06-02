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
import Json.Decode as D
import Json.Encode as Encode
import LeaderBoard exposing (LeaderBoard)
import Outgoing
import Ports exposing (messageReceiver, sendMessage)
import Quiz exposing (Answer, Question, Quiz)
import Review
import Route exposing (Route(..))
import Score exposing (Score)
import Set exposing (Set)
import Url exposing (Url)
import User exposing (User(..))



-- QUIZ


type Game
    = WaitingToPlay
    | Playing Turn
    | ViewingResults


type Turn
    = Started
    | LockedIn
    | WaitingForFriends
    | ReadyForNextTurn



-- INIT


init : flags -> Url -> Key -> ( Model, Cmd msg )
init _ url key =
    let
        model =
            { key = key
            , errorMessage = Nothing
            , user = User.anonymous
            , userDraft = ""

            -- QUIZ
            , game = WaitingToPlay
            , leaderBoard =
                LeaderBoard.empty
            , quiz = Quiz.second

            -- INTER-APP COMMS
            , socket = Nothing
            , sockets = Set.empty
            , users = Dict.empty
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
    , leaderBoard : LeaderBoard

    -- Navigation
    , key : Navigation.Key

    -- Inter-app communication
    , socket : Maybe String
    , sockets : Set String
    , users : Dict String String
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


type PortMsg
    = LeaderBoardMsg LeaderBoard
    | ExitMsg String
    | EnterMsg String
    | JoinMsg Channel String
    | UserMsg Channel String String


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
                (D.field "payload" (D.field "id" D.string))

        "user" ->
            D.map3 UserMsg
                (D.field "channel" channelDecoder)
                (D.field "payload" (D.field "user" D.string))
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
            case route of
                Index ->
                    init () url model.key

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
                | game = Playing Started
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
                cmd =
                    Encode.object
                        [ ( "channel", Encode.string "public" )
                        , ( "type", Encode.string "quiz" )
                        , ( "payload"
                          , Encode.object
                                [ ( "id", encodeSocket model.socket )
                                , ( "quiz", Quiz.encodeQuiz model.quiz )
                                ]
                          )
                        ]
                        |> Ports.sendMessage
            in
            ( { model | game = Playing LockedIn }, cmd )

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

                        JoinMsg channel str ->
                            ( { model
                                | sockets = Set.insert str model.sockets
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

                        ExitMsg str ->
                            ( { model
                                | sockets = Set.remove str model.sockets
                                , users = Dict.remove str model.users
                              }
                            , Cmd.none
                            )

                Err error ->
                    -- TODO report errors
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { body = [ viewBody model ]
    , title = "The Quiet Ryan's"
    }


viewBody : Model -> Html Msg
viewBody model =
    let
        friends =
            Dict.values model.users |> List.sort
    in
    case model.game of
        WaitingToPlay ->
            viewStartPage model.userDraft friends

        Playing turn ->
            let
                _ =
                    Debug.log "turn" turn

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
                    [ class "flex-grow"
                    , class "flex"
                    , class "justify-center"
                    , class "items-center"
                    ]
                    [ Quiz.viewQuestion SelectAnswer question
                    ]
                , viewQuiz model.quiz
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
                    , "space-y-2"
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
                            , "mb-2"
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
                    , primaryButtonStyle
                    , class "w-full"
                    ]
                    [ text "Start Quiz!" ]

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


viewQuiz : Quiz -> Html Msg
viewQuiz quiz =
    let
        previous =
            Quiz.getPrevious quiz

        question =
            Quiz.getQuestion quiz

        remaining =
            Quiz.getNext quiz
    in
    case previous of
        [] ->
            div [ Container.style ]
                [ -- Navigation buttons
                  div
                    [ class "flex justify-end"
                    , class "pb-8"
                    ]
                    [ if Quiz.answered question then
                        lockInButton LockAnswerIn

                      else
                        text ""
                    ]
                ]

        _ ->
            case remaining of
                [] ->
                    div [ Container.style ]
                        [ -- Navigation buttons
                          div [ class "flex justify-end" ]
                            [ previousButton
                            , finishButton
                            ]
                        ]

                _ ->
                    div [ Container.style ]
                        [ -- Navigation buttons
                          div [ class "flex justify-end" ]
                            [ previousButton
                            , nextButton (not (Quiz.answered question))
                            ]
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


waitingForFriendsButton : Html Msg
waitingForFriendsButton =
    button
        [ class <|
            String.join " " <|
                [ "bg-purple-500"
                , "text-white"
                , "flex-grow"
                , "uppercase"
                , "px-4"
                , "py-6"
                , "mx-2"
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
            , div [ class "pl-2" ] [ text "Waiting for friends..." ]
            ]
        ]


nextButton : Bool -> Html Msg
nextButton notAnswered =
    button
        [ primaryButtonStyle
        , class "bg-green-500"
        , class "flex-grow"
        , onClick NextQuestion
        , disabled notAnswered
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


finishButton : Html Msg
finishButton =
    button
        [ primaryButtonStyle
        , class "flex-grow"
        , onClick FinishQuiz
        ]
        [ text "Finish" ]


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
