{-
   Main program, note only comments can exist before the
   module statement
-}


port module Main exposing (main, portDecoder)

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
import Page.New as New
import Quiz exposing (Answer, Question, Quiz)
import Route exposing (Route(..))
import Url



-- MODEL


type Model
    = Model Key Page


type Page
    = IndexPage IndexModel
    | NewPage New.Model


type alias IndexModel =
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


type User
    = Anonymous
    | LoggedIn String



-- QUIZ


type Game
    = WaitingToPlay
    | Playing
    | ViewingResults


type LeaderBoard
    = LeaderBoard (List Score)


type Score
    = Score User Int



-- MSG


type Msg
    = IndexMsg IndexMsg
      -- PORT
    | Recv Value
      -- NAVIGATION
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type IndexMsg
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


type Status
    = NotStarted
    | Opened
    | Closed



-- INIT


type alias Flags =
    { route : Maybe String }


init : D.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init value url key =
    let
        -- TODO use flags.route
        flags =
            D.decodeValue decoderFlags value

        route =
            Route.fromUrl url

        _ =
            Debug.log "route" route
    in
    case route of
        Route.Index ->
            let
                page =
                    IndexPage
                        { key = key
                        , url = url
                        , draft = ""
                        , messages = []
                        , status = NotStarted
                        , errorMessage = Nothing
                        , user = Anonymous
                        , userDraft = ""

                        -- QUIZ
                        , game = WaitingToPlay
                        , leaderBoard =
                            LeaderBoard []
                        , quiz = Quiz.first
                        }
            in
            ( Model key page, Cmd.none )

        Route.New ->
            let
                page =
                    NewPage New.init
            in
            ( Model key page, Cmd.none )

        Route.Quiz ->
            -- TODO support this route
            let
                page =
                    NewPage New.init
            in
            ( Model key page, Cmd.none )


decoderFlags : D.Decoder Flags
decoderFlags =
    D.map Flags
        (D.maybe (D.field "route" D.string))



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model key page) =
    let
        model =
            Model key page
    in
    case msg of
        -- NAVIGATION
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl key
                        (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( model, Cmd.none )

        -- PORT
        Recv value ->
            case page of
                IndexPage subModel ->
                    case D.decodeValue portDecoder value of
                        Ok leaderBoard ->
                            let
                                ( indexModel, cmd ) =
                                    updateIndex (GotLeaderBoard leaderBoard) subModel
                            in
                            ( Model key (IndexPage indexModel), cmd )

                        Err error ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        -- PAGE
        IndexMsg subMsg ->
            case page of
                IndexPage subModel ->
                    let
                        ( indexModel, cmd ) =
                            updateIndex subMsg subModel
                    in
                    ( Model key (IndexPage indexModel), cmd )

                _ ->
                    ( model, Cmd.none )


updateIndex : IndexMsg -> IndexModel -> ( IndexModel, Cmd Msg )
updateIndex msg model =
    case msg of
        DraftChanged str ->
            ( { model | draft = str }, Cmd.none )

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

        -- LEADERBOARD
        GotLeaderBoard leaderBoard ->
            ( { model | leaderBoard = leaderBoard }, Cmd.none )

        -- QUIZ
        StartQuiz ->
            ( { model
                | game = Playing
                , user = LoggedIn model.userDraft
              }
            , Browser.Navigation.pushUrl model.key "/quiz"
            )

        FinishQuiz ->
            let
                score =
                    Score model.user (Quiz.tally model.quiz)

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
            ( { model | quiz = Quiz.nextQuestion model.quiz }, Cmd.none )

        PreviousQuestion ->
            ( { model | quiz = Quiz.previousQuestion model.quiz }, Cmd.none )

        SelectAnswer answer ->
            ( { model | quiz = Quiz.selectAnswer answer model.quiz }, Cmd.none )

        NoOp ->
            -- TEMPORARY TO PASS COMPILER
            ( model, Cmd.none )



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


view : Model -> Browser.Document Msg
view model =
    { body = [ Html.map IndexMsg (viewBody model) ]
    , title = "The Quiet Ryan's"
    }


viewBody : Model -> Html IndexMsg
viewBody (Model key page) =
    case page of
        IndexPage model ->
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

        _ ->
            -- TODO Branch pages
            New.view


viewError : Maybe D.Error -> Html IndexMsg
viewError maybeError =
    case maybeError of
        Nothing ->
            text ""

        Just error ->
            div [] [ text (D.errorToString error) ]


viewStartPage : String -> Html IndexMsg
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


viewLeaderBoard : LeaderBoard -> Html IndexMsg
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


viewScore : Score -> Html IndexMsg
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


viewUser : User -> Html IndexMsg
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


quizContainerStyle : Html.Attribute msg
quizContainerStyle =
    class "max-w-sm pb-2 shadow-lg my-5 mx-2 rounded bg-gray-100"


viewQuiz : Quiz -> Html IndexMsg
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
            div [ quizContainerStyle ]
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
                    div [ quizContainerStyle ]
                        [ Quiz.viewQuestion SelectAnswer question

                        -- Navigation buttons
                        , div [ class "flex justify-end" ]
                            [ previousButton
                            , finishButton
                            ]
                        ]

                _ ->
                    div [ quizContainerStyle ]
                        [ Quiz.viewQuestion SelectAnswer question

                        -- Navigation buttons
                        , div [ class "flex justify-end" ]
                            [ previousButton
                            , nextButton (not (Quiz.answered question))
                            ]

                        -- Remaining questions info
                        , viewRemaining remaining
                        ]


viewRemaining : List Question -> Html IndexMsg
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


primaryButtonStyle : Html.Attribute IndexMsg
primaryButtonStyle =
    class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded mx-2 my-2"


previousButton : Html IndexMsg
previousButton =
    button
        [ class "bg-white border border-blue-500 hover:bg-blue-700 text-near-black py-2 px-4 rounded mx-2 my-2"
        , onClick PreviousQuestion
        ]
        [ text "Go back" ]


nextButton : Bool -> Html IndexMsg
nextButton notAnswered =
    button
        [ primaryButtonStyle
        , onClick NextQuestion
        , disabled notAnswered
        ]
        [ text "Next" ]


finishButton : Html IndexMsg
finishButton =
    button
        [ primaryButtonStyle
        , onClick FinishQuiz
        ]
        [ text "Finish" ]



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



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv
