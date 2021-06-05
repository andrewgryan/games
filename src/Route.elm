module Route exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Url.Parser as Parser
    exposing
        ( (</>)
        , Parser
        , int
        , map
        , oneOf
        , s
        , top
        )


type Route
    = Index
    | Admin


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault Index (Parser.parse parser url)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Index top
        , map Admin (s "admin")
        ]


toString : Route -> String
toString route =
    case route of
        Index ->
            "/"

        Admin ->
            "/admin"
