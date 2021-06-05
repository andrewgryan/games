module Quiz exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Quiz
    = Quiz (List Question) Question (List Question)


type Question
    = Question String (List Answer)
    | Answered String (List Answer) Answer


type Answer
    = Right String
    | Wrong String



-- DECODE


decoder : Decoder Quiz
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "QUIZ" ->
                        Decode.map3 Quiz
                            (Decode.field "previous" (Decode.list questionDecoder))
                            (Decode.field "current" questionDecoder)
                            (Decode.field "remaining" (Decode.list questionDecoder))

                    _ ->
                        Decode.fail "Could not decode quiz"
            )


questionDecoder : Decoder Question
questionDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "QUESTION" ->
                        Decode.map2 Question
                            (Decode.field "statement" Decode.string)
                            (Decode.field "answers" (Decode.list answerDecoder))

                    "ANSWERED" ->
                        Decode.map3 Answered
                            (Decode.field "statement" Decode.string)
                            (Decode.field "answers" (Decode.list answerDecoder))
                            (Decode.field "answer" answerDecoder)

                    _ ->
                        Decode.fail "Failed to decode question"
            )


answerDecoder : Decoder Answer
answerDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "RIGHT" ->
                        Decode.map Right
                            (Decode.field "value" Decode.string)

                    "WRONG" ->
                        Decode.map Wrong
                            (Decode.field "value" Decode.string)

                    _ ->
                        Decode.fail "Could not decode answer"
            )



-- ENCODE


encodeQuiz : Quiz -> Encode.Value
encodeQuiz quiz =
    case quiz of
        Quiz previous current remaining ->
            Encode.object
                [ ( "type", Encode.string "QUIZ" )
                , ( "previous", Encode.list encodeQuestion previous )
                , ( "current", encodeQuestion current )
                , ( "remaining", Encode.list encodeQuestion remaining )
                ]


encodeQuestion : Question -> Encode.Value
encodeQuestion question =
    case question of
        Question str answers ->
            Encode.object
                [ ( "type", Encode.string "QUESTION" )
                , ( "statement", Encode.string str )
                , ( "answers", Encode.list encodeAnswer answers )
                ]

        Answered str answers answer ->
            Encode.object
                [ ( "type", Encode.string "ANSWERED" )
                , ( "statement", Encode.string str )
                , ( "answers", Encode.list encodeAnswer answers )
                , ( "answer", encodeAnswer answer )
                ]


encodeAnswer : Answer -> Encode.Value
encodeAnswer answer =
    case answer of
        Right str ->
            Encode.object
                [ ( "type", Encode.string "RIGHT" )
                , ( "value", Encode.string str )
                ]

        Wrong str ->
            Encode.object
                [ ( "type", Encode.string "WRONG" )
                , ( "value", Encode.string str )
                ]



-- HELPERS


getPrevious : Quiz -> List Question
getPrevious (Quiz previous _ _) =
    previous


getQuestion : Quiz -> Question
getQuestion (Quiz _ question _) =
    question


getQuestionIndex : Quiz -> Int
getQuestionIndex (Quiz previous _ _) =
    List.length previous


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


getStatement : Question -> String
getStatement question =
    case question of
        Question str _ ->
            str

        Answered str _ _ ->
            str



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
            List.reverse previous ++ [ current ] ++ next


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


second : Quiz
second =
    Quiz
        []
        (Question "How many loaves in a bakers dozen?"
            [ Wrong "12"
            , Right "13"
            , Wrong "14"
            ]
        )
        [ Question "How many points did the UK get in the Eurovision?"
            [ Wrong "12 - From Ireland"
            , Right "0 - Nul points"
            , Wrong "523 - One short of Italy"
            ]
        , Question "Which year did Arthur Guinness found Guiness?"
            [ Wrong "1066"
            , Wrong "1812"
            , Right "1759"
            ]
        , Question "How many pairs of wings does a bee have?"
            [ Wrong "one"
            , Right "two"
            , Wrong "three"
            ]
        , Question "Yuriy Gagarin was the first man in space, who was the first American in space?"
            [ Wrong "Buzz Aldrin"
            , Wrong "Chuck Yeager"
            , Right "Alan Shephard"
            ]
        , Question "Which of these planets has the hottest atmosphere?"
            [ Wrong "Mercury"
            , Right "Venus"
            , Wrong "Earth"
            , Wrong "Mars"
            ]
        , Question "What does a funambulist do?"
            [ Right "Walk a tight rope"
            , Wrong "Juggle"
            , Wrong "Follows ambulances"
            ]
        , Question "I'm tall when I'm young and short when I'm old, what am I?"
            [ Wrong "Fuse"
            , Wrong "Giraffe"
            , Right "Candle"
            ]
        , Question "What is a group of turkeys called?"
            [ Wrong "A clutch"
            , Right "A rafter"
            , Wrong "A brood"
            , Wrong "A peep"
            ]
        , Question "Complete the lyric: Fly me to the moon, let me..."
            [ Right "...play among the stars"
            , Wrong "...see what life is like on Jupiter and Mars"
            , Wrong "...entertain you"
            ]
        ]


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
