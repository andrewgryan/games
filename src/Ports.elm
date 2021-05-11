port module Ports exposing (messageReceiver, sendMessage)

import Json.Encode exposing (Value)



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (Value -> msg) -> Sub msg
