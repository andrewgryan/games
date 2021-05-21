module Page.New exposing (Model, Msg, init, update, view)

import Helper exposing (classes)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MSG


type Msg
    = DraftChanged String



-- MODEL


type Model
    = Draft String



--INIT


init : Model
init =
    Draft "Hello"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged str ->
            ( Draft str, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Draft str ->
            div
                [ classes
                    [ "grid"
                    , "min-h-screen"
                    , "bg-gray-200"
                    , "justify-center"
                    , "content-center"
                    ]
                ]
                [ div
                    [ classes
                        [ "p-4"
                        , "bg-white"
                        , "rounded"
                        , "shadow-lg"
                        ]
                    ]
                    [ h1
                        [ classes
                            [ "font-bold"
                            , "pb-4"
                            , "text-xl"
                            ]
                        ]
                        [ text "Hello, Vite" ]
                    , viewInput "Statement"
                    , div
                        [ classes []
                        ]
                        [ label [] [ text "Options" ]
                        , viewOption
                        , viewOption
                        , viewOption
                        , button
                            [ classes
                                [ "bg-green-700"
                                , "p-2"
                                , "rounded"
                                , "w-50"
                                ]
                            ]
                            [ text "+" ]
                        ]
                    ]
                ]


viewInput : String -> Html Msg
viewInput str =
    div
        [ classes
            [ "py-2"
            ]
        ]
        [ label
            [ classes
                [ "font-sm"
                ]
            ]
            [ text str ]
        , input
            [ classes
                [ "block"
                , "border"
                , "border-solid-gray"
                , "my-1"
                ]
            , onInput DraftChanged
            ]
            []
        ]


viewOption : Html Msg
viewOption =
    div
        [ classes
            []
        ]
        [ input
            [ classes
                [ "border"
                , "border-solid-gray"
                , "my-1"
                , "mr-2"
                ]
            , onInput DraftChanged
            ]
            []
        , input
            [ classes
                []
            , type_ "checkbox"
            ]
            []
        ]
