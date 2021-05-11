module User exposing (..)

import Json.Decode as D



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


decoder : D.Decoder User
decoder =
    D.map LoggedIn D.string


toString : User -> String
toString user =
    case user of
        Anonymous ->
            "Anonymous"

        LoggedIn str ->
            str
