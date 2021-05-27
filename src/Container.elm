module Container exposing (style)

import Helper exposing (classes)
import Html exposing (Attribute)


style : Attribute msg
style =
    classes
        [ "max-w-sm"
        , "pb-2"
        , "my-5"
        , "mx-2"
        , "flex-grow"
        , "flex"
        , "flex-col"
        , "justify-between"
        ]
