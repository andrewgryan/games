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
    | Room Int


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault Index (Parser.parse parser url)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Index top
        , map Room (s "room" </> int)
        ]


toString : Route -> String
toString route =
    case route of
        Index ->
            "/"

        Room n ->
            "/room/" ++ String.fromInt n
