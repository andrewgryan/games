port module Ports exposing (encode, messageReceiver, sendMessage)

import Json.Decode as Decode
import Json.Encode as Encode



-- PORTS


port sendMessage : Decode.Value -> Cmd msg


port messageReceiver : (Encode.Value -> msg) -> Sub msg



-- Encode


encode : String -> Encode.Value -> String
encode type_ payload =
    Encode.encode 0
        (Encode.object
            [ ( "type", Encode.string type_ )
            , ( "payload", payload )
            ]
        )
