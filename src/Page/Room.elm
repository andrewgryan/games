module Page.Room exposing (..)

import Browser.Navigation as Navigation exposing (Key)
import Header
import Helper exposing (classes)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route exposing (Route(..))
import Url


type Msg
    = GotDraft String
    | GoTo Route


type alias Model =
    { id : ID
    , userName : String
    , key : Key
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


init : Key -> Int -> Model
init key n =
    { id = fromInt n
    , userName = ""
    , key = key
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDraft str ->
            ( { model | userName = str }, Cmd.none )

        GoTo route ->
            ( model, Navigation.pushUrl model.key (Route.toString route) )



-- VIEW


view : Model -> Html Msg
view model =
    let
        id =
            toInt model.id
    in
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
        [ Header.view
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
                [ "bg-teal-600"
                , "text-white"
                , "uppercase"
                , "m-4"
                , "p-4"
                , "box-border"
                ]
            , onClick (GoTo (Play id))
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
            "Room " ++ id

        _ ->
            "Hi " ++ userName ++ ", nice to meet you..."
