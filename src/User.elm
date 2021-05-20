module User exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- USER


type User
    = Anonymous
    | LoggedIn String



-- CONSTRUCT


anonymous : User
anonymous =
    Anonymous


loggedIn : String -> User
loggedIn str =
    LoggedIn str



-- DECODE


decoder : Decoder User
decoder =
    Decode.map LoggedIn Decode.string



-- ENCODE


encode : User -> Encode.Value
encode user =
    Encode.string (toString user)


toString : User -> String
toString user =
    case user of
        Anonymous ->
            "Anonymous"

        LoggedIn str ->
            str
