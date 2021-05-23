module Outgoing exposing (..)

import Json.Encode as Encode exposing (string)
import Score exposing (Score)
import User exposing (User)


type Outgoing
    = SaveScore Score
    | Answer User
    | JoinRoom User


save : Score -> Outgoing
save score =
    SaveScore score


answer : User -> Outgoing
answer user =
    Answer user


joinRoom : User -> Outgoing
joinRoom user =
    JoinRoom user


encode : Outgoing -> String
encode outgoing =
    case outgoing of
        JoinRoom user ->
            Encode.encode 0
                (Encode.object
                    [ ( "type", Encode.string "join" )
                    , ( "payload"
                      , Encode.object
                            [ ( "user", User.encode user )
                            , ( "room", Encode.string "101" )
                            ]
                      )
                    ]
                )

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
                    , ( "payload", Score.encode score )
                    ]
                )
