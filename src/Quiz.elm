module Quiz exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Quiz
    = Quiz (List Question) Question (List Question)


type Question
    = Question String (List Answer)
    | Answered String (List Answer) Answer


type Answer
    = Right String
    | Wrong String



-- HELPERS


getPrevious : Quiz -> List Question
getPrevious (Quiz previous _ _) =
    previous


getQuestion : Quiz -> Question
getQuestion (Quiz _ question _) =
    question


getNext : Quiz -> List Question
getNext (Quiz _ _ next) =
    next


answered : Question -> Bool
answered question =
    case question of
        Question _ _ ->
            False

        Answered _ _ _ ->
            True



-- MOVE


nextQuestion : Quiz -> Quiz
nextQuestion quiz =
    case quiz of
        Quiz previous current next ->
            case next of
                [] ->
                    quiz

                question :: remaining ->
                    Quiz (current :: previous) question remaining


previousQuestion : Quiz -> Quiz
previousQuestion quiz =
    case quiz of
        Quiz previous current next ->
            case previous of
                [] ->
                    quiz

                question :: remaining ->
                    Quiz remaining question (current :: next)


selectAnswer : Answer -> Quiz -> Quiz
selectAnswer answer quiz =
    case quiz of
        Quiz previous question next ->
            Quiz previous (updateQuestion question answer) next


updateQuestion : Question -> Answer -> Question
updateQuestion question answer =
    case question of
        Question statement answers ->
            Answered statement answers answer

        Answered statement answers _ ->
            Answered statement answers answer



-- SCORE


tally : Quiz -> Int
tally quiz =
    quiz
        |> allQuestions
        |> List.map toScore
        |> computeScore


computeScore : List Int -> Int
computeScore scores =
    List.foldr (+) 0 scores


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



-- VIEW


viewQuestion : (Answer -> msg) -> Question -> Html msg
viewQuestion toMsg question =
    let
        styles =
            [ class "flex"
            , class "flex-col"
            , class "mx-2"
            , class "w-full"
            ]

        ulStyles =
            [ class "space-y-2"
            ]
    in
    case question of
        Question statement answers ->
            div styles
                [ -- Question
                  viewStatement statement

                -- Answers
                , ul ulStyles (List.map (viewAnswer toMsg) answers)
                ]

        Answered statement answers answer ->
            div styles
                [ -- Question
                  viewStatement statement

                -- Answers
                , ul ulStyles (List.map (viewAnswered toMsg answer) answers)
                ]


viewStatement : String -> Html msg
viewStatement statement =
    div
        [ class "font-bold"
        , class "text-lg"
        , class "pb-4"
        ]
        [ text statement ]


viewAnswer : (Answer -> msg) -> Answer -> Html msg
viewAnswer toMsg answer =
    li
        [ styleAnswer False
        , class "bg-gray-200"
        , class "p-4"
        , onClick (toMsg answer)
        ]
        [ text (answerToString answer) ]


viewAnswered : (Answer -> msg) -> Answer -> Answer -> Html msg
viewAnswered toMsg selected answer =
    li
        [ styleAnswer (selected == answer)
        , class "bg-gray-200"
        , class "p-4"
        , onClick (toMsg answer)
        ]
        [ text (answerToString answer) ]


styleAnswer : Bool -> Html.Attribute msg
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



-- WRITTEN QUIZ


first : Quiz
first =
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

        -- 3
        , Question "What is the capital of Australia?"
            [ Wrong "Sydney"
            , Wrong "Melbourne"
            , Right "Canberra"
            ]

        -- 4
        , Question "What is the largest island in the Meditteranean?"
            [ Wrong "Corsica"
            , Right "Sicily"
            , Wrong "Cyprus"
            ]

        -- 5
        , Question "Do you want to continue watching Netflix?"
            [ Right "Yes"
            , Right "No"
            ]

        -- 6
        , Question "Who painted the ceiling of the Sistine Chapel in Rome?"
            [ Wrong "Leonardo"
            , Wrong "Donatello"
            , Right "Michelangelo"
            , Wrong "Raphael"
            ]

        -- 7
        , Question "Complete the lyric: It's been a hard day's night, I should be..."
            [ Wrong "...working like a dog"
            , Right "...sleeping like a log"
            , Wrong "...a paperback writer"
            ]

        -- 8
        , Question "How white was Mary's lamb's fleece?"
            [ Wrong "Nutmeg"
            , Wrong "Almond"
            , Wrong "Blossom"
            , Wrong "Chiffon"
            , Wrong "Metal"
            , Right "Snow"
            ]

        -- 9
        , Question "Which of the following, was NOT one of Christopher Columbus's ships?"
            [ Wrong "Pinta"
            , Wrong "Santa Maria"
            , Right "Beagle"
            , Wrong "Nina"
            ]

        -- 10
        , Question "In bowling, what are 3 consecutive strikes called?"
            [ Wrong "An eagle"
            , Wrong "A birdie"
            , Right "A turkey"
            , Wrong "A parrot"
            ]

        -- 11
        , Question "What's the highest mountain in Africa?"
            [ Wrong "Mount Kenya"
            , Wrong "Mount Stanley"
            , Right "Mount Kilmanjaro"
            ]

        -- 12
        , Question "What's unique about Mozambiques flag?"
            [ Right "It has an AK-47 on it"
            , Wrong "It has no national flag"
            , Wrong "It's not rectangular"
            ]
        ]
