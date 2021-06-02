module Socket.ID exposing (..)


type ID
    = ID String


toString : ID -> String
toString (ID str) =
    str
