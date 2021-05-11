module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s, top)


type Route
    = Index
    | Quiz
    | New


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault Index (Parser.parse parser url)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Index top
        , map Quiz (s "quiz")
        , map New (s "new")
        ]
