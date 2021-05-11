module LeaderBoard exposing (..)

import Container
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Score exposing (Score)


type LeaderBoard
    = LeaderBoard (List Score)



-- CONSTRUCT


empty : LeaderBoard
empty =
    LeaderBoard []



-- DECODE


decoder : D.Decoder LeaderBoard
decoder =
    D.map LeaderBoard
        (D.list Score.decoder)


view : LeaderBoard -> Html msg
view leaderBoard =
    case leaderBoard of
        LeaderBoard scores ->
            div
                [ Container.style
                ]
                [ h1
                    [ class "font-bold p-4"
                    ]
                    [ text "Score board" ]
                , div
                    [ class "px-4 pb-4"
                    ]
                    (List.map Score.view scores)
                ]
