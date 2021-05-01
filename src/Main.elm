{-
   Main program, note only comments can exist before the
   module statement
-}


port module Main exposing (main, portDecoder)

import Browser
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
        , placeholder
        , type_
        , value
        )
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
    , quiz : Quiz
    , game : Game
    , leaderBoard : LeaderBoard
    }


type User
    = Anonymous
    | LoggedIn String



-- QUIZ


type Game
    = WaitingToPlay
    | Playing
    | ViewingResults


type Quiz
    = Quiz (List Question) Question (List Question)


type Question
    = Question String (List Answer)
    | Answered String (List Answer) Answer


type Answer
    = Right String
    | Wrong String


type LeaderBoard
    = LeaderBoard (List Score)


type Score
    = Score User Int



-- MSG


type Msg
    = DraftChanged String
    | Recv Value
    | WebSocket Status
      -- USER
    | UserSend
    | UserDraftChanged String
      -- QUIZ
    | StartQuiz
    | NextQuestion
    | PreviousQuestion
    | FinishQuiz
    | SelectAnswer Answer


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
      , game = WaitingToPlay
      , leaderBoard =
            LeaderBoard []
      , quiz =
            Quiz
                []
                (Question "How many pennies in a shilling?"
                    [ Right "12"
                    , Wrong "24"
                    , Wrong "1/2"
                    ]
                )
                [ Question "Who invented dynamite?"
                    [ Right "Alfred Nobel"
                    , Wrong "Thomas Edison"
                    , Wrong "Isambard Kingdom Brunel"
                    ]
                , Question "What is the capital of Australia?"
                    [ Wrong "Sydney"
                    , Wrong "Melbourne"
                    , Right "Canberra"
                    ]
                , Question "What is the largest island in the Meditteranean?"
                    [ Wrong "Corsica"
                    , Right "Sicily"
                    , Wrong "Cyprus"
                    ]
                , Question "Do you want to exit Netflix?"
                    [ Right "Yes"
                    , Wrong "No"
                    ]
                ]
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged str ->
            ( { model | draft = str }, Cmd.none )

        Recv value ->
            case D.decodeValue portDecoder value of
                Ok leaderBoard ->
                    ( { model | leaderBoard = leaderBoard }, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just error }, Cmd.none )

        WebSocket status ->
            ( { model | status = status }, Cmd.none )

        -- USER
        UserSend ->
            ( { model
                | userDraft = ""
                , user = LoggedIn model.userDraft
              }
            , Cmd.none
            )

        UserDraftChanged str ->
            ( { model | userDraft = str }, Cmd.none )

        -- QUIZ
        StartQuiz ->
            ( { model
                | game = Playing
                , user = LoggedIn model.userDraft
              }
            , Cmd.none
            )

        FinishQuiz ->
            let
                tally =
                    model.quiz
                        |> allQuestions
                        |> List.map toScore
                        |> computeScore

                score =
                    Score model.user tally

                cmd =
                    SaveScore score
                        |> portEncoder
                        |> sendMessage
            in
            ( { model
                | game = ViewingResults
                , leaderBoard = LeaderBoard []
              }
            , cmd
            )

        NextQuestion ->
            case model.quiz of
                Quiz previous current next ->
                    case next of
                        [] ->
                            ( model, Cmd.none )

                        question :: remaining ->
                            ( { model | quiz = Quiz (current :: previous) question remaining }, Cmd.none )

        PreviousQuestion ->
            case model.quiz of
                Quiz previous current next ->
                    case previous of
                        [] ->
                            ( model, Cmd.none )

                        question :: remaining ->
                            ( { model | quiz = Quiz remaining question (current :: next) }, Cmd.none )

        SelectAnswer answer ->
            case model.quiz of
                Quiz previous question next ->
                    let
                        quiz =
                            Quiz previous (updateQuestion question answer) next
                    in
                    ( { model | quiz = quiz }, Cmd.none )


updateQuestion : Question -> Answer -> Question
updateQuestion question answer =
    case question of
        Question statement answers ->
            Answered statement answers answer

        Answered statement answers _ ->
            Answered statement answers answer


allQuestions : Quiz -> List Question
allQuestions quiz =
    case quiz of
        Quiz previous current next ->
            previous ++ [ current ] ++ next


toScore : Question -> Int
toScore question =
    case question of
        Answered _ _ answer ->
            case answer of
                Right _ ->
                    1

                Wrong _ ->
                    0

        Question _ _ ->
            0


computeScore : List Int -> Int
computeScore scores =
    List.foldr (+) 0 scores



-- PORT DECODER


portDecoder : D.Decoder LeaderBoard
portDecoder =
    D.map LeaderBoard
        (D.list scoreDecoder)


scoreDecoder : D.Decoder Score
scoreDecoder =
    D.map2 Score
        (D.field "user" userDecoder)
        (D.field "score" D.int)


userDecoder : D.Decoder User
userDecoder =
    D.map LoggedIn D.string



-- D.oneOf
--     [ D.field "data" D.string |> D.andThen (\s -> D.succeed (Ack s))
--     , D.field "error" D.string |> D.andThen (\s -> D.succeed (Nack s))
--     ]


type Outgoing
    = SaveScore Score


portEncoder : Outgoing -> String
portEncoder outgoing =
    case outgoing of
        SaveScore score ->
            Json.Encode.encode 0
                (Json.Encode.object
                    [ ( "type", Json.Encode.string "score" )
                    , ( "payload", encodeScore score )
                    ]
                )


encodeScore : Score -> Json.Encode.Value
encodeScore score =
    case score of
        Score user n ->
            Json.Encode.object
                [ ( "user", Json.Encode.string (toString user) )
                , ( "score", Json.Encode.int n )
                ]



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
                , viewLeaderBoard model.leaderBoard
                ]


viewError : Maybe D.Error -> Html Msg
viewError maybeError =
    case maybeError of
        Nothing ->
            text ""

        Just error ->
            div [] [ text (D.errorToString error) ]


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
            , button
                [ onClick StartQuiz
                , primaryButtonStyle
                ]
                [ text "Start Quiz!" ]
            ]
        ]


viewLeaderBoard : LeaderBoard -> Html Msg
viewLeaderBoard leaderBoard =
    case leaderBoard of
        LeaderBoard scores ->
            div
                [ quizContainerStyle
                ]
                [ h1
                    [ class "font-bold p-4"
                    ]
                    [ text "Score board" ]
                , div
                    [ class "px-4 pb-4"
                    ]
                    (List.map viewScore scores)
                ]


viewScore : Score -> Html Msg
viewScore score =
    case score of
        Score user tally ->
            div []
                [ text (toString user ++ ": " ++ String.fromInt tally)
                ]


toString : User -> String
toString user =
    case user of
        Anonymous ->
            "Anonymous"

        LoggedIn str ->
            str


viewUser : User -> Html Msg
viewUser user =
    div
        [ class "font-light text-sm p-1"
        ]
        [ text "Signed in as "
        , span
            [ class "font-bold"
            ]
            [ text (toString user) ]
        ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Closed ->
            div [] [ text "Connection lost" ]

        _ ->
            div [] []


quizContainerStyle : Html.Attribute Msg
quizContainerStyle =
    class "max-w-sm pb-2 shadow-lg my-5 mx-2 rounded bg-gray-100"


viewQuiz : Quiz -> Html Msg
viewQuiz (Quiz previous question remaining) =
    case previous of
        [] ->
            div [ quizContainerStyle ]
                [ viewQuestion question

                -- Navigation buttons
                , div [ class "flex justify-end" ]
                    [ nextButton
                    ]
                ]

        _ ->
            case remaining of
                [] ->
                    div [ quizContainerStyle ]
                        [ viewQuestion question

                        -- Navigation buttons
                        , div [ class "flex justify-end" ]
                            [ previousButton
                            , finishButton
                            ]
                        ]

                _ ->
                    div [ quizContainerStyle ]
                        [ viewQuestion question

                        -- Navigation buttons
                        , div [ class "flex justify-end" ]
                            [ previousButton
                            , nextButton
                            ]
                        ]


viewQuestion : Question -> Html Msg
viewQuestion question =
    case question of
        Question statement answers ->
            div []
                [ -- Question
                  viewStatement statement

                -- Answers
                , ul [] (List.map viewAnswer answers)
                ]

        Answered statement answers answer ->
            div []
                [ -- Question
                  viewStatement statement

                -- Answers
                , ul [] (List.map (viewAnswered answer) answers)
                ]


viewStatement : String -> Html Msg
viewStatement statement =
    div [ class "p-2 font-bold" ] [ text statement ]


primaryButtonStyle : Html.Attribute Msg
primaryButtonStyle =
    class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded mx-2 my-2"


previousButton : Html Msg
previousButton =
    button
        [ class "bg-white border border-blue-500 hover:bg-blue-700 text-near-black py-2 px-4 rounded mx-2 my-2"
        , onClick PreviousQuestion
        ]
        [ text "Go back" ]


nextButton : Html Msg
nextButton =
    button
        [ primaryButtonStyle
        , onClick NextQuestion
        ]
        [ text "Next" ]


finishButton : Html Msg
finishButton =
    button
        [ primaryButtonStyle
        , onClick FinishQuiz
        ]
        [ text "Finish" ]


viewAnswer : Answer -> Html Msg
viewAnswer answer =
    li
        [ styleAnswer False
        , onClick (SelectAnswer answer)
        ]
        [ text (answerToString answer) ]


viewAnswered : Answer -> Answer -> Html Msg
viewAnswered selected answer =
    li
        [ styleAnswer (selected == answer)
        , onClick (SelectAnswer answer)
        ]
        [ text (answerToString answer) ]


styleAnswer : Bool -> Html.Attribute Msg
styleAnswer selected =
    let
        common =
            "px-4 hover:text-white hover:bg-blue-500 cursor-pointer"
    in
    if selected then
        class ("font-bold " ++ common)

    else
        class common


answerToString : Answer -> String
answerToString answer =
    case answer of
        Right str ->
            str

        Wrong str ->
            str



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
