module Outgoing exposing (encode, save)

import Json.Encode exposing (string)
import Score exposing (Score)


type Outgoing
    = SaveScore Score


save : Score -> Outgoing
save score =
    SaveScore score


encode : Outgoing -> String
encode outgoing =
    case outgoing of
        SaveScore score ->
            Json.Encode.encode 0
                (Json.Encode.object
                    [ ( "type", Json.Encode.string "score" )
                    , ( "payload", Score.encode score )
                    ]
                )
