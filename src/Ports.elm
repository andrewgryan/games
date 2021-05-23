port module Ports exposing (encode, messageReceiver, sendMessage)

import Json.Encode as Encode exposing (Value)



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (Value -> msg) -> Sub msg



-- Encode


encode : String -> Value -> String
encode type_ payload =
    Encode.encode 0
        (Encode.object
            [ ( "type", Encode.string type_ )
            , ( "payload", payload )
            ]
        )
