module Score exposing (..)

import Html exposing (..)
import Json.Decode as D
import Json.Encode
import User exposing (User)


type Score
    = Score User Int



-- CONSTRUCT


fromInt : User -> Int -> Score
fromInt user n =
    Score user n



-- DECODE


decoder : D.Decoder Score
decoder =
    D.map2 Score
        (D.field "user" User.decoder)
        (D.field "score" D.int)



-- ENCODE


encode : Score -> Json.Encode.Value
encode score =
    case score of
        Score user n ->
            Json.Encode.object
                [ ( "user", Json.Encode.string (User.toString user) )
                , ( "score", Json.Encode.int n )
                ]



-- VIEW


view : Score -> Html msg
view score =
    case score of
        Score user tally ->
            div []
                [ text (User.toString user ++ ": " ++ String.fromInt tally)
                ]
