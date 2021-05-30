module Review exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Quiz exposing (Answer(..), Question(..), Quiz, allQuestions)



-- VIEW


view : Quiz -> Html msg
view quiz =
    div
        [ class <|
            String.join " " <|
                [ "px-4"
                , "flex"
                , "flex-col"
                , "space-y-2"
                ]
        ]
        (List.map viewQuestion (allQuestions quiz))


viewQuestion : Question -> Html msg
viewQuestion question =
    case question of
        Question statement answers ->
            div
                [ class <|
                    String.join " " <|
                        [ "py-4"
                        , "font-bold"
                        ]
                ]
                [ text statement
                , div [] (List.map viewAnswer answers)
                ]

        Answered statement answers answer ->
            div
                [ class <|
                    String.join " " <|
                        [ "py-4"
                        , "font-bold"
                        ]
                ]
                [ text statement
                , div [] (List.map (viewAnswered answer) answers)
                ]


viewAnswer : Answer -> Html msg
viewAnswer answer =
    case answer of
        Right str ->
            div
                [ class "bg-green-200"
                ]
                [ text str ]

        Wrong str ->
            div
                [ class "bg-red-200"
                ]
                [ text str ]


viewAnswered : Answer -> Answer -> Html msg
viewAnswered user option =
    if option == user then
        -- VIEW USER SELECTED CHOICE
        case option of
            Right str ->
                div
                    [ class "bg-green-400"
                    , class "text-white"
                    , class "font-bold"
                    ]
                    [ text str ]

            Wrong str ->
                div
                    [ class "bg-red-400"
                    , class "text-white"
                    , class "font-bold"
                    ]
                    [ text str ]

    else
        -- VIEW RIGHT/WRONG ANSWER
        case option of
            Right str ->
                div
                    [ class "font-bold"
                    , class "text-green-500"
                    ]
                    [ text str ]

            Wrong str ->
                div
                    [ class "bg-white"
                    , class "font-normal"
                    ]
                    [ text str ]
