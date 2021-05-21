module Page.Index exposing (..)

import Browser.Navigation exposing (Key)
import Container
import Helper exposing (ifIsEnter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D
import LeaderBoard exposing (LeaderBoard)
import Outgoing
import Ports exposing (sendMessage)
import Quiz exposing (Answer, Question, Quiz)
import Score exposing (Score)
import Url exposing (Url)
import User exposing (User)



-- QUIZ


type Game
    = WaitingToPlay
    | Playing
    | ViewingResults



-- SOCKET STATUS


type Status
    = NotStarted
    | Opened
    | Closed



-- INIT


init : Key -> Url -> Model
init key url =
    { key = key
    , url = url
    , draft = ""
    , messages = []
    , status = NotStarted
    , errorMessage = Nothing
    , user = User.anonymous
    , userDraft = ""

    -- QUIZ
    , game = WaitingToPlay
    , leaderBoard =
        LeaderBoard.empty
    , quiz = Quiz.first
    }



-- MODEL


type alias Model =
    { user : User
    , userDraft : String
    , draft : String
    , messages : List String
    , status : Status
    , errorMessage : Maybe D.Error
    , quiz : Quiz
    , game : Game
    , leaderBoard : LeaderBoard

    -- Navigation
    , key : Browser.Navigation.Key
    , url : Url.Url
    }



-- MSG


type Msg
    = NoOp
    | DraftChanged String
    | WebSocket Status
      -- USER
    | UserSend
    | UserDraftChanged String
      --LEADERBOARD
    | GotLeaderBoard LeaderBoard
      -- QUIZ
    | StartQuiz
    | NextQuestion
    | PreviousQuestion
    | FinishQuiz
    | SelectAnswer Answer


gotLeaderBoard : LeaderBoard -> Msg
gotLeaderBoard board =
    GotLeaderBoard board



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged str ->
            ( { model | draft = str }, Cmd.none )

        WebSocket status ->
            ( { model | status = status }, Cmd.none )

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

        -- LEADERBOARD
        GotLeaderBoard leaderBoard ->
            ( { model | leaderBoard = leaderBoard }, Cmd.none )

        -- QUIZ
        StartQuiz ->
            ( { model
                | game = Playing
                , user = User.loggedIn model.userDraft
              }
            , Browser.Navigation.pushUrl model.key "/quiz"
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

        NoOp ->
            -- TEMPORARY TO PASS COMPILER
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.game of
        WaitingToPlay ->
            viewStartPage model.userDraft

        Playing ->
            div []
                [ -- QUIZ
                  viewQuiz model.quiz
                , viewUser model.user
                ]

        ViewingResults ->
            div []
                [ viewError model.errorMessage
                , LeaderBoard.view model.leaderBoard
                ]


viewStartPage : String -> Html Msg
viewStartPage draftName =
    div
        [ class "w-full max-w-xs"
        ]
        [ div
            [ class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4"
            ]
            [ div
                [ class "mb-4"
                ]
                [ label
                    [ class "block text-gray-700 text-sm font-bold mb-2"
                    ]
                    [ text "Enter name:" ]
                , input
                    [ type_ "text"
                    , placeholder "Write here"
                    , onInput UserDraftChanged
                    , on "keydown" (ifIsEnter StartQuiz)
                    , value draftName
                    , class "shadow appearence-none border rounded-w-full py-2 px-3"
                    ]
                    []
                ]
            , if draftName /= "" then
                button
                    [ onClick StartQuiz
                    , primaryButtonStyle
                    ]
                    [ text "Start Quiz!" ]

              else
                text ""
            ]
        ]


primaryButtonStyle : Html.Attribute Msg
primaryButtonStyle =
    class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded mx-2 my-2"


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
                [ Quiz.viewQuestion SelectAnswer question

                -- Navigation buttons
                , div [ class "flex justify-end" ]
                    [ nextButton (not (Quiz.answered question))
                    ]

                -- Remaining questions info
                , viewRemaining remaining
                ]

        _ ->
            case remaining of
                [] ->
                    div [ Container.style ]
                        [ Quiz.viewQuestion SelectAnswer question

                        -- Navigation buttons
                        , div [ class "flex justify-end" ]
                            [ previousButton
                            , finishButton
                            ]
                        ]

                _ ->
                    div [ Container.style ]
                        [ Quiz.viewQuestion SelectAnswer question

                        -- Navigation buttons
                        , div [ class "flex justify-end" ]
                            [ previousButton
                            , nextButton (not (Quiz.answered question))
                            ]

                        -- Remaining questions info
                        , viewRemaining remaining
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
        [ class "bg-white border border-blue-500 hover:bg-blue-700 text-near-black py-2 px-4 rounded mx-2 my-2"
        , onClick PreviousQuestion
        ]
        [ text "Go back" ]


nextButton : Bool -> Html Msg
nextButton notAnswered =
    button
        [ primaryButtonStyle
        , onClick NextQuestion
        , disabled notAnswered
        ]
        [ text "Next" ]


finishButton : Html Msg
finishButton =
    button
        [ primaryButtonStyle
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
