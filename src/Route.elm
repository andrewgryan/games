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
    | Quiz
    | New
    | Room Int


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault Index (Parser.parse parser url)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Index top
        , map Quiz (s "quiz")
        , map New (s "new")
        , map Room (s "room" </> int)
        ]


toString : Route -> String
toString route =
    case route of
        Index ->
            "/"

        Quiz ->
            "/"

        New ->
            "/"

        Room n ->
            "/room/" ++ String.fromInt n
