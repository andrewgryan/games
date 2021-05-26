module Page.Room exposing (..)

import Browser.Navigation as Navigation
import Helper exposing (classes)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url


type Msg
    = GotDraft String


type alias Model =
    { id : ID
    , userName : String
    }



-- ROOM ID


type ID
    = ID Int


fromInt : Int -> ID
fromInt n =
    ID n


toInt : ID -> Int
toInt (ID n) =
    n


toString : ID -> String
toString (ID n) =
    String.fromInt n



-- INIT


init : Int -> Model
init n =
    { id = fromInt n, userName = "" }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDraft str ->
            ( { model | userName = str }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ classes
            [ "bg-white"
            , "h-screen"
            , "min-w-screen"
            , "flex"
            , "flex-col"
            , "justify-between"
            ]
        ]
        [ div
            [ classes
                [ "bg-gray-900"
                , "text-white"
                , "text-xl"
                , "text-center"
                , "p-4"
                ]
            ]
            [ text "Header" ]
        , div
            [ classes
                [ "bg-white"
                , "m-4"
                , "p-4"
                , "text-sm"
                ]
            ]
            [ div []
                [ text "Enter name:"
                ]
            , input
                [ onInput GotDraft
                , type_ "text"
                , classes
                    [ "border"
                    , "border-gray-200"
                    , "bg-gray-200"
                    , "py-2"
                    , "px-4"
                    , "w-full"
                    , "text-lg"
                    , "focus:bg-white"
                    , "focus:border-purple-500"
                    ]
                ]
                []
            ]
        , div
            [ classes
                [ "bg-white"
                , "text-xl"
                , "text-center"
                , "p-4"
                ]
            ]
            [ text (greet model.id model.userName) ]
        , button
            [ classes
                [ "bg-blue-600"
                , "text-white"
                , "uppercase"
                , "m-4"
                , "p-4"
                , "box-border"
                ]
            ]
            [ text "Start Quiz"
            ]
        ]


greet : ID -> String -> String
greet roomId userName =
    let
        id =
            toString roomId
    in
    case userName of
        "" ->
            "Welcome to Room " ++ id ++ "!"

        _ ->
            "Hi " ++ userName ++ ", nice to meet you..."
