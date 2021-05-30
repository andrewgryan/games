module Main exposing (main)

import Browser
import Browser.Navigation as Navigation exposing (Key)
import Container
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
import User exposing (User)



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

            -- QUIZ
            , game = WaitingToPlay
            , leaderBoard =
                LeaderBoard.empty
            , quiz = Quiz.second

            -- INTER-APP COMMS
            , socket = Nothing
            , sockets = Set.empty
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
    }



-- MSG


type Msg
    = -- USER
      UserSend
    | UserDraftChanged String
      -- QUIZ
    | StartQuiz
    | NextQuestion
    | PreviousQuestion
    | FinishQuiz
    | SelectAnswer Answer
      -- PORT
    | Recv Encode.Value
      -- NAVIGATION
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type PortMsg
    = LeaderBoardMsg LeaderBoard
    | ExitMsg String
    | EnterMsg String
    | JoinMsg String



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
            D.map JoinMsg (D.field "payload" (D.field "id" D.string))

        "exit" ->
            D.map ExitMsg (D.field "payload" (D.field "id" D.string))

        _ ->
            D.fail "Unrecognised msg type"


joinRoomPayload : Int -> Encode.Value
joinRoomPayload n =
    Encode.object
        [ ( "room", Encode.int n )
        ]


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

        -- USER
        UserSend ->
            ( { model
                | userDraft = ""
                , user = User.loggedIn model.userDraft
              }
            , Cmd.none
            )

        UserDraftChanged str ->
            ( { model | userDraft = str }, Cmd.none )

        -- QUIZ
        StartQuiz ->
            let
                user =
                    User.loggedIn model.userDraft
            in
            ( { model
                | game = Playing
                , user = user
              }
            , Cmd.none
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
    case model.game of
        WaitingToPlay ->
            viewStartPage model.userDraft

        Playing ->
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
                    [ viewUser model.user
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
            [ "bg-blue-500"
            , "hover:bg-blue-700"
            , "text-white"
            , "font-bold"
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
                    , class "bg-red-200"
                    ]
                    [ nextButton (not (Quiz.answered question))
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


nextButton : Bool -> Html Msg
nextButton notAnswered =
    button
        [ primaryButtonStyle
        , class "flex-grow"
        , onClick NextQuestion
        , disabled notAnswered
        ]
        [ text "Next" ]


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
