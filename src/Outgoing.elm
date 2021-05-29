module Outgoing exposing (..)

import Json.Encode as Encode exposing (string)
import Score exposing (Score)
import User exposing (User)


type Outgoing
    = SaveScore Score
    | Answer User


save : Score -> Outgoing
save score =
    SaveScore score


answer : User -> Outgoing
answer user =
    Answer user


encode : Outgoing -> String
encode outgoing =
    case outgoing of
        Answer user ->
            Encode.encode 0
                (Encode.object
                    [ ( "type", Encode.string "answer" )
                    , ( "payload"
                      , Encode.object
                            [ ( "user", User.encode user )
                            ]
                      )
                    ]
                )

        SaveScore score ->
            Encode.encode 0
                (Encode.object
                    [ ( "type", Encode.string "score" )
                    , ( "channel", Encode.string "score" )
                    , ( "payload", Score.encode score )
                    ]
                )
